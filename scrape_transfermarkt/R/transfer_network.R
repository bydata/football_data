library(tidyverse)
library(ggtext)
library(ggforce)
library(ggraph)
library(tidygraph)
library(colorspace)
library(networkD3)


transfers <- read_rds("output/transfers_20201212134210.RData")
team_mapping <- read_rds("output/team_mapping.RData")
team_neighbor_mapping <- read_csv("output/team_neighbor_mapping.csv")


# fonts
library(extrafont)
loadfonts()

# create a dataframe of connections (i.e. transfers)
connections <- transfers %>% 
  map(rename, club.name = 7, club.league = 8) %>% 
  bind_rows(.id = "direction") %>%
  # keep only transfers and loans
  filter(transfer_type %in% c("Transfer", "Loan", "Other")) %>% 
  # # keep only teams which played in Bundesliga in particular season
  # inner_join(team_mapping, by = c("team.name" = "name", "year")) %>% 
  # streamline club names for selling/receiving teams
  left_join(team_neighbor_mapping, by = c("club.name" = "x")) %>% 
  mutate(club.name = ifelse(!is.na(closest), closest, club.name)) %>% 
  mutate(from = ifelse(direction == "outs", team.name, club.name),
         to = ifelse(direction == "adds", team.name, club.name)) %>% 
  # generate transfer key to remove duplicates (i.e. transfers within Bundesliga will have 2 entries)
  mutate(key = str_c(player.name, from, to, year, sep = "#")) %>% 
  distinct(from, to, player.name, year, fee)
  

# calculate connection weights (i.e. the number of transfers and transfer volume)
connections_weighted <- connections %>% 
  group_by(from, to) %>% 
  summarize(volume = sum(fee), transfers_n = n()) %>% 
  mutate(volume = replace_na(volume, 0))

# count how many times a team has been involved in a transfers
team_freq <- connections %>% 
  select(from, to) %>% 
  pivot_longer(cols = c(from, to), names_to = "direction", values_to = "team") %>% 
  count(team, sort = TRUE)


# create graph from connections
graph <- as_tbl_graph(connections_weighted, directed = TRUE) 

# add number of teams being involved in transfers
graph <- graph %>% 
  activate(nodes) %>% 
  left_join(team_freq, by = c("name" = "team"))


selected_team <- "Borussia Dortmund" 
#selected_team <- "RasenBallsport Leipzig"

team_graph <- graph %>% 
  activate(edges) %>% 
  filter(from == which(.N()$name == selected_team) | 
           to == which(.N()$name == selected_team)) %>%
  #filter(transfers_n > 1 | volume > 1E6) %>% 
  mutate(direction_selected = ifelse(
    from == which(.N()$name == selected_team), 
    "out", 
    "add")) %>% 
  activate(nodes) %>% 
  filter(row_number() %in% .E()$from |
           row_number() %in% .E()$to) %>% 
  mutate(foo = sum(.E()$from[which(.N()$name == name)]))

x11(width = 10, height = 7)

team_graph %>% 
  ggraph(layout = "nicely") +
  geom_edge_bend(aes(filter = transfers_n > 1, edge_width = transfers_n,
                     edge_color = direction_selected), 
                 #edge_color = "grey70", 
                 #arrow = arrow(angle = 20, length = unit(0.25, "cm"))
                 ) +
  geom_node_point(aes(size = n, 
                      shape = (name == selected_team))) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.1, 4)) +
  facet_edges(vars(direction_selected)) +
  theme_graph() +
  theme(legend.position = "top")


bvb_graph <- team_graph
graph_d3 <- list(
  links = bvb_graph %>% 
    activate(edges) %>% 
    as_tibble() %>% 
    # subtract 1 from since JS is zero-indexed
    mutate_at(vars(from, to), ~ . - 1) %>% 
    as.data.frame(),
  nodes = bvb_graph %>% 
    activate(nodes) %>% 
    as_tibble() %>% 
    as.data.frame()
)

# forceNetwork(Links = foo2$links, Nodes = foo2$nodes, 
#              Source = 'from', Target = 'to', 
#              NodeID = 'name', Group = 'name',
#              Nodesize = "n", linkWidth = "volume",
#              fontFamily = "Open Sans")

forceNetwork(Links = graph_d3$links, Nodes = graph_d3$nodes, 
             Source = 'from', Target = 'to', 
             NodeID = 'name', Group = 'name')




## )

glimpse(connections)


team_transfer_network <- function(team, year_from = 2014, seed = 123) {
  
  team_connections <- connections %>% 
    filter(to == team) %>% 
    filter(year >= year_from) %>%
    filter(from != to) %>% 
    filter(from != "RB Leipzig II") %>% 
    group_by(to, from) %>% 
    summarize(volume = sum(fee, na.rm = TRUE), transfers_n = n(),
              .groups = "drop") %>% 
    mutate(volume = replace_na(volume, 0)) %>% 
    arrange(desc(transfers_n), .groups = "drop")
  
  team_graph <- as_tbl_graph(team_connections, directed = TRUE) %>% 
    activate(nodes) %>% 
    left_join(team_connections, by = c("name" = "from")) %>% 
    select(-to) %>% 
    # replace NA for selected team with arbitrary value
    mutate(volume = replace_na(volume, sum(volume, na.rm = TRUE) / n()),
           # volume = replace_na(volume, 10^7),
           transfers_n = replace_na(transfers_n, 10))
  
  set.seed(seed)
  p <- team_graph %>% 
    ggraph(layout = "nicely") +
    geom_edge_bend(aes(edge_width = transfers_n), edge_color = "grey75") +
    geom_node_point(aes(size = volume, fill = (name == team | transfers_n > 8)), 
                    shape = 21, col = "white") +
    geom_node_text(aes(filter = transfers_n > 0, label = name), 
                   repel = TRUE, size = 2,
                   family = "Source Sans Pro Light",
                   col = "grey20") +
    # geom_node_text(aes(filter = transfers_n > 5 & name != team, 
    #                    label = transfers_n), 
    #                repel = FALSE, size = 3,
    #                family = "Source Sans Pro SemiBold",
    #                col = "grey20") +
    scale_size(range = c(1, 10), limits = c(0, 1.2 * 10^8), breaks = seq(0, 1.2 * 10^8, 10^7)) +
    scale_edge_width_continuous(range = c(0.05, 4), limits = c(1, 20), breaks = seq(0, 20, 5)) +
    scale_fill_discrete_qualitative(palette = "Harmonic") +
    guides(size = FALSE, fill = FALSE, edge_width = FALSE) +
    labs(subtitle = "Je dicker die Linie zu einem Club, desto mehr Spieler wurden von dort transferiert.<br>
         Je größer der Punkt des abgebenden Teams, desto größer das Transfervolumen (in EUR).",
         caption = glue::glue("Transfers seit {year_from}. Quelle: Eigene Darstellung  @4nsgarW nach transfermarkt.de")
         ) +
    theme_graph(base_family = "Source Sans Pro Light", base_size = 10) +
    theme(legend.position = "top",
          legend.justification = "left",
          text = element_text(color = "grey20"),
          plot.title = element_markdown(family = "Source Sans Pro SemiBold", color = "black", 
                                        size = 12, lineheight = 1.2),
          plot.subtitle = element_markdown(lineheight = 1.2, size = 8),
          plot.caption = element_markdown(hjust = 0, size = 7))
  
  print(p)
}


selected_teams <- c("RasenBallsport Leipzig", "Borussia Dortmund", "FC Bayern München", "Bayer 04 Leverkusen")
selected_teams <- c("RasenBallsport Leipzig")
walk(selected_teams, 
     ~ggsave(str_c("Transfer Network_", .x, ".png"), 
             plot = team_transfer_network(.x), 
             type = "cairo", dpi = 200, width = 6, height = 4))

team_transfer_network("RasenBallsport Leipzig", year_from = 2014) +
  labs(title = "Seit 2014 wurden 15 Profis von Red Bull Salzburg<br>zu Red Bull Leipzig transferiert")
ggsave(str_c("Transfer Network_", "RasenBallsport Leipzig", ".png"), 
       type = "cairo", dpi = 200, width = 6, height = 4.5)

team <- "FC Bayern München"
team_transfer_network(team, year_from = 2014) +
  labs(title = "Zum Vergleich: Transfers von Bayern München seit 2014")
ggsave(str_c("Transfer Network_", team, ".png"), 
       type = "cairo", dpi = 200, width = 6, height = 4.5)

team <- "Borussia Dortmund"
team_transfer_network(team, year_from = 2014) +
  labs(title = "Zum Vergleich: Transfers von Borussia Dortmund seit 2014")
ggsave(str_c("Transfer Network_", team, ".png"), 
       type = "cairo", dpi = 200, width = 6, height = 4.5)

team <- "SV Werder Bremen"
team_transfer_network(team, year_from = 2014) +
  labs(title = "Zum Vergleich: Transfers von Werder Bremen seit 2014")
ggsave(str_c("Transfer Network_", team, ".png"), 
       type = "cairo", dpi = 200, width = 6, height = 4.5)
