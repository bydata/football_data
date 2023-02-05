library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)
library(treemapify)  

base_path <- "transfer-fees-2022-23"

countries <- c("England", "Spain", "Germany", "Italy", "France")

if (FALSE) {
  team_urls <- map(countries, tm_league_team_urls, start_year = 2022)
  team_urls_top5 <- reduce(team_urls, c)
  
  transfers <- map(team_urls_top5, tm_team_transfers, transfer_window = "all")
  write_rds(transfers, here(base_path, "data", "transfers-2022-23.rds"))
} else {
  transfers <- read_rds(here(base_path, "data", "transfers-2022-23.rds"))
}
transfers_df <- bind_rows(transfers)

league_pal <- c("#1B998B", "#ED217C", "#2D3047", "#8D2962", "#FF9B71")
league_pal <- c("Premier League" = "#A8201A", "Bundesliga" = "#143642",
                "LaLiga" = "#0F8B8D", "Ligue 1" = "#CA5D22", "Serie A" = "#EC9A29")
league_pal_epl_highlighted <- c(unname(league_pal["Premier League"]), 
                                colorspace::desaturate(league_pal[2:5], 0.6))
# colorspace::lighten(colorspace::desaturate(league_pal, 0.6), 0.2)
league_pal_epl_highlighted_greyscale <- c(unname(colorspace::desaturate(league_pal["Premier League"], 0.6)), 
                                       "grey30", "grey45", "grey60", "grey75")

transfers_df %>% 
  filter(transfer_type == "Arrivals" & !is.na(transfer_fee)) %>% 
  mutate(
    player_name = fct_reorder(player_name, transfer_fee),
    league = factor(league, levels = c("Premier League", "Bundesliga", "LaLiga", "Ligue 1", "Serie A"))
    ) %>% 
  slice_max(order_by = transfer_fee, n = 20) %>% 
  ggplot(aes(player_name, transfer_fee)) +
  geom_col(aes(fill = league), width = 0.8) +
  geom_text(
    aes(label = player_name, y = 1e6),
    family = "Roboto Condensed", fontface = "bold", hjust = 0, col = "white", size = 2.5) +
  geom_text(
    aes(label = scales::number(transfer_fee, scale = 1e-6, suffix = "M", accuracy = 0.1)),
    family = "Roboto Condensed", hjust = 1.1, col = "white", size = 2.5) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"),
                     expand = c(0, 0)) +
  scale_fill_manual(values = league_pal) +
  coord_flip() +
  labs(
    title = "Top 20 transfers in 2022/'23",
    subtitle = "Most expensive transfers in European top 5 leagues",
    caption = "Source: transfermarkt.de, {worldfootballR} package. Visualisation: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
ggsave(here(base_path, "plots", "transfers-top20.png"), width = 5, height = 4)

transfers_df %>% 
  filter(transfer_type == "Arrivals" & !is.na(transfer_fee)) %>% 
  mutate(
    player_name = fct_reorder(player_name, transfer_fee),
    league = factor(league, levels = c("Premier League", "Bundesliga", "LaLiga", "Ligue 1", "Serie A")),
    league_or_chelsea = ifelse(team_name == "Chelsea FC", team_name, as.character(league)),
    league_or_chelsea = factor(
      league_or_chelsea, 
      levels = c("Chelsea FC", "Premier League", "Bundesliga", "LaLiga", "Ligue 1", "Serie A"))) %>% 
  slice_max(order_by = transfer_fee, n = 20) %>% 
  ggplot(aes(player_name, transfer_fee)) +
  geom_col(aes(fill = league_or_chelsea), width = 0.8) +
  geom_text(
    aes(label = player_name, y = 1e6),
    family = "Roboto Condensed", fontface = "bold", hjust = 0, col = "white", size = 2.5) +
  geom_text(
    aes(label = scales::number(transfer_fee, scale = 1e-6, suffix = "M", accuracy = 0.1)),
    family = "Roboto Condensed", hjust = 1.1, col = "white", size = 2.5) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"),
                     expand = c(0, 0)) +
  scale_fill_manual(values = unname(c(league_pal["Premier League"], league_pal_epl_highlighted_greyscale))) +
  coord_flip() +
  labs(
    title = "Top 20 transfers in 2022/'23",
    subtitle = "Most expensive transfers in European top 5 leagues",
    caption = "Source: transfermarkt.de, {worldfootballR} package. Visualisation: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
ggsave(here(base_path, "plots", "transfers-top20-chelsea-highlighted.png"), width = 5, height = 4)



## Beeswarm plot all transfers ================================================

transfers_df %>% 
  # filter(transfer_type == "Arrivals" & !is.na(transfer_fee) & transfer_fee > 0) %>% 
  filter(transfer_type == "Arrivals" & !is.na(transfer_fee)) %>% 
  mutate(
    league = factor(league, levels = c("Premier League", "Bundesliga", "LaLiga", "Ligue 1", "Serie A"))) %>% 
  ggplot(aes(x = 1, y = transfer_fee)) +
  ggbeeswarm::geom_quasirandom(size = 1, shape = 21, color = "white", fill = "grey20") +
  coord_flip() +
  facet_wrap(vars(league), ncol = 1) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "grey24"),
    plot.title = element_text(color = "black")
  )
# ggsave(here(base_path, "plots", "transfers-top20.png"), width = 5, height = 4)


beeswarm_df <- transfers_df %>% 
  filter(transfer_type == "Arrivals" & !is.na(transfer_fee)) %>% 
  group_by(league, team_name) %>% 
  summarize(total_transfer_fee = sum(transfer_fee), .groups = "drop") %>% 
  mutate(
    league = factor(league, levels = c("Premier League", "Bundesliga", "LaLiga", "Ligue 1", "Serie A"))) 

beeswarm_df %>% 
  slice_max(order_by = total_transfer_fee, n = 20) %>% 
  head(20)

p_team_beeswarm <- beeswarm_df %>% 
  ggplot(aes(x = 1, y = total_transfer_fee)) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = league),
    size = 2, shape = 21, color = "white") +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"),
                     expand = c(0, 0)) +
  scale_fill_manual(values = league_pal) +
  coord_flip(clip = "off") +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = 4))) +
  labs(
    title = "Chelsea spent a lot more than any other team",
    subtitle = "Transfer spendings of teams in European top 5 leagues",
    x = NULL,
    y = "Total transfer fees (in EUR)",
    fill = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top",
    plot.title.position = "plot",
    text = element_text(color = "grey24"),
    plot.title = element_text(color = "black")
  )

label_positions <- layer_data(p_team_beeswarm, 1) %>% 
  slice_max(order_by = y, n = 20) %>% 
  inner_join(beeswarm_df, by = c("y" = "total_transfer_fee"))
  
p_team_beeswarm + 
  ggrepel::geom_text_repel(
    data = ~subset(label_positions, team_name %in% c("Chelsea FC", "Manchester United", "West Ham United",
                                       "FC Barcelona", "Paris Saint-Germain")),
    aes(x = x, y = y, label = team_name),
    inherit.aes = FALSE,
    size = 2, color = "grey40", family = "Roboto Condensed", 
    min.segment.length = 0, segment.size = 0.1
  )
ggsave(here(base_path, "plots", "transfers-teams-beeswarm.png"), width = 5, height = 4)





## Balance - arrivals and departure volumes ====================================

transfers_df %>% 
  filter(!is.na(transfer_fee)) %>% 
  # filter(league == "Bundesliga") %>% # "Premier League"
  mutate(transfer_fee_diverging = ifelse(transfer_type == "Arrivals", 1, -1) * transfer_fee) %>%
  group_by(league, team_name, transfer_type) %>% 
  summarize(transfer_fee_diverging = sum(transfer_fee_diverging), .groups = "drop") %>% 
  # mutate(team_name = fct_reorder(team_name, transfer_fee_diverging)) %>% 
  mutate(team_name = tidytext::reorder_within(team_name, transfer_fee_diverging, within = league)) %>% 
  ggplot(aes(team_name, transfer_fee_diverging)) +
  geom_col(aes(alpha = transfer_type, fill = league),
           width = 0.7) +
  # geom_text(aes(x = as.numeric(team_name) + 0.4, y = 0, label = team_name), 
  #           hjust = 1, size = 2.5) +
  geom_hline(aes(yintercept = 0), color = "white", size = 0.5) +
  tidytext::scale_x_reordered() +
  scale_fill_manual(values = league_pal) +
  scale_alpha_manual(values = c(1, 0.7)) +
  coord_flip() +
  facet_wrap(vars(league), scales = "free_y", strip.position = "top", ncol = 1) +
  guides(fill = "none") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    strip.text = element_text(hjust = 0, face = "bold", angle = 0),
    strip.placement = "outside",
    strip.clip = "off",
    plot.title.position = "plot",
    panel.grid.major = element_line(color = "grey70", linetype = "dotted", 
                                    linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 6),
    legend.position = "top"
  )
ggsave(here(base_path, "plots", "transfers-balance-facet.png"), width = 6, height = 9)




theme_treemap <- function(base_family = "Roboto Condensed", ...) {
  theme_void(base_family = base_family) +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      text = element_text(color = "grey24"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      plot.margin = margin(t = 2, b = 8, l = 4, r = 4)
    )
}



transfers_tmp_df <- transfers_df %>% 
  filter(transfer_type == "Arrivals") %>% 
  group_by(window, league, team_name) %>% 
  summarize(total_transfer_fee = sum(transfer_fee, na.rm = TRUE), .groups = "drop") %>% 
  arrange(-total_transfer_fee) %>% 
  mutate(
    label = sprintf(
      "**%s**<br>%s",
      team_name, 
      scales::number(total_transfer_fee, scale = 1e-6, suffix = "M EUR", accuracy = 0.1))
  ) %>% 
  mutate(
    league = factor(league, levels = c("Premier League", "Bundesliga", "LaLiga", "Ligue 1", "Serie A")))


transfers_tmp_df %>% 
  ggplot(aes(area = total_transfer_fee, subgroup = league, 
             subgroup2 = toupper(label), fill = league)) +
  geom_treemap(col = "white") +
  geom_treemap_subgroup_border(col = "white") +
  geom_treemap_subgroup_text(
    aes(),
    col = "white", family = "Roboto Condensed") +
  scale_fill_manual(values = league_pal) +
  guides(fill = "none") +
  theme_treemap()
ggsave(here(base_path, "plots", "transfers-treemap-leagues-all.png"), width = 6, height = 6)




p_tmp_facetted <- transfers_tmp_df %>% 
  ggplot(aes(area = total_transfer_fee, subgroup = league, 
             subgroup2 = toupper(label), fill = league)) +
  geom_treemap(col = "white") +
  geom_treemap_subgroup_border(col = "white") +
  geom_treemap_subgroup_text(
    col = "white", family = "Roboto Condensed") +
  geom_treemap_subgroup2_text(
    col = "white", family = "Roboto Condensed", place = "topleft", grow = FALSE) +
  facet_wrap(vars(window), scales = "fixed") +
  guides(fill = "none") +
  theme_treemap()


# with 3 subgroup levels to preserve correct areas between summer and winter
p_tmp_facetted <- transfers_tmp_df %>% 
ggplot(aes(area = total_transfer_fee, subgroup = window, subgroup2 = toupper(league), 
           subgroup3 = toupper(label), fill = league)) +
  geom_treemap(col = "white", size = 0.2) +
  geom_treemap_subgroup_border(col = "white", size = 1, linetype = "solid") +
  geom_treemap_subgroup2_border(col = "white") +
  geom_treemap_subgroup2_text(
    col = "white", family = "Roboto Condensed") +
  geom_treemap_subgroup3_text(
    col = "grey90", family = "Roboto Condensed", place = "topleft", grow = FALSE) +
  guides(fill = "none") +
  theme_treemap()

p_tmp_facetted + 
  scale_fill_manual(values = league_pal) 

p_tmp_facetted + 
  scale_fill_manual(values = league_pal_epl_highlighted) 

ggsave(here(base_path, "plots", "transfers-treemap-leagues-facetted.png"), width = 8, height = 6)



walk(
  c("Summer", "Winter"),
  function(x) {
    transfers_tmp_df %>% 
      filter(window == x) %>% 
      ggplot(aes(area = total_transfer_fee, subgroup = league, 
                 subgroup2 = toupper(label), fill = league)) +
      geom_treemap(col = "white") +
      geom_treemap_subgroup_border(col = "white") +
      geom_treemap_subgroup_text(
        col = "white", family = "Roboto Condensed") +
      geom_treemap_subgroup2_text(
        col = "white", family = "Roboto Condensed", place = "topleft", grow = FALSE) + 
      scale_fill_manual(values = c(unname(league_pal["Premier League"]), 
                                   colorspace::desaturate(league_pal[2:5], 0.5))) +
      guides(fill = "none") +
      theme_treemap()
    filename <- here(base_path, "plots", sprintf("transfers-treemap-leagues-facetted-%s.png", x))
    ggsave(filename, width = 8, height = 6)
  }
)



### ---------

# Custom function for string wrapping which uses <br> tags for linebreaks
str_wrap_html <- function (string, width = 80, indent = 0, exdent = 0) {
  if (width <= 0) 
    width <- 1
  out <- stringi::stri_wrap(string, width = width, indent = indent, 
                   exdent = exdent, simplify = FALSE)
  vapply(out, str_c, collapse = "<br>", character(1))
}

show_label_threshold <- 41e6


transfers_tmp_df %>% 
  group_by(window, league) %>% 
  summarize(total_transfer_fee = sum(total_transfer_fee), .groups = "drop")

67035000 + 32080000 + 130200000 + 32220000

transfers_tmp_df <- transfers_df %>% 
  filter(transfer_type == "Arrivals") %>% 
  group_by(window, league, team_name) %>% 
  summarize(total_transfer_fee = sum(transfer_fee, na.rm = TRUE), .groups = "drop") %>% 
  arrange(-total_transfer_fee) %>% 
  rowwise() %>% 
  mutate(
    team_name_wrapped = str_wrap_html(team_name, total_transfer_fee / 3.5e6),
    label = sprintf(
      "**%s**<br>%s",
      team_name_wrapped, 
      scales::number(total_transfer_fee, scale = 1e-6, suffix = "M EUR", accuracy = 0.1)),
    label = ifelse(total_transfer_fee >= show_label_threshold, label, ""),
    league = factor(league, levels = c("Premier League", "Bundesliga", "LaLiga", "Ligue 1", "Serie A"))
    ) %>% 
  ungroup()

# Create a dataset with treemap dimensions
treemap_df <- treemapify(
  transfers_tmp_df,
  area = "total_transfer_fee",
  subgroup = "window",
  subgroup2 = "league",
  subgroup3 = "team_name",
  layout = "squarified"
) %>% 
  group_by(window, league, team_name) %>% 
  mutate(
    subgroup2_xmin = min(xmin),
    subgroup2_xmax = max(xmax),
    subgroup2_ymin = min(ymin),
    subgroup2_ymax = max(ymax)
  ) %>% 
  ungroup() %>% 
  group_by(window, league) %>% 
  mutate(
    subgroup_xmin = min(xmin),
    subgroup_xmax = max(xmax),
    subgroup_ymin = min(ymin),
    subgroup_ymax = max(ymax)
  ) %>% 
  ungroup() 

treemap_df %>% 
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(aes(fill = league), color = NA, size = 0) +
  # league borders
  geom_rect(
    aes(xmin = subgroup_xmin, xmax = subgroup_xmax,
        ymin = subgroup_ymin, ymax = subgroup_ymax),
    color = "white", fill = NA, size = 0.5) +
  # team borders
  geom_rect(
    aes(xmin = subgroup2_xmin, xmax = subgroup2_xmax,
        ymin = subgroup2_ymin, ymax = subgroup2_ymax),
    color = "white", fill = NA, 
    size = 0.2) +
  # ... only Chelsea
  geom_rect(
    data = . %>% filter(team_name == "Chelsea FC"),
    aes(xmin = subgroup2_xmin, xmax = subgroup2_xmax,
        ymin = subgroup2_ymin, ymax = subgroup2_ymax),
    color = "white",  fill = league_pal["Premier League"], 
    size = 0.2) +
  geom_richtext(
    data = . %>% group_by(window, team_name, label) %>% 
      summarize(xmin = min(xmin), ymax = max(ymax), .groups = "drop") %>% 
      filter(),
    aes(x = xmin, y = ymax, label = label),
    color = "white",
    inherit.aes = FALSE, hjust = 0, vjust = 1, fill = NA, label.size = 0,
    family = "Roboto Condensed", lineheight = 1, size = 2.5) +
  facet_grid(cols = vars(window), space = "free", scales = "free_x") +
  scale_y_continuous(expand = c(0.025, 0)) +
  scale_fill_manual(values = league_pal_epl_highlighted_greyscale) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  labs(
    title = sprintf("<b style='color:%s'>Chelsea FC</b> spent most of 
    all clubs in top 5 leagues in 2022-23.
    In the winter transfer window, they spent more than La Liga, Bundesliga,
    Ligue 1 and Serie A combined.", league_pal["Premier League"]),
    subtitle = "Total transfers amounts per team in 2022-23",
    caption = "Source: transfermarkt.de, {worldfootballR} package. Visualisation: Ansgar Wolsing",
    fill = NULL) +
  theme_treemap() +
  theme(
    legend.key.width = unit(4, "mm"),
    legend.key.height = unit(2, "mm"),
    plot.title = element_textbox(width = 1, face = "plain", size = 16, lineheight = 1.2)
  )
ggsave(here(base_path, "plots", "transfers-treemap-leagues-facetted.png"), width = 8.5, height = 7)





## Transfer amounts as a circlepack / hierarchicalgraph ==============================

library(ggraph)
library(igraph)

transfers_tmp_df_season <- transfers_tmp_df %>% 
  group_by(league, team_name) %>% 
  summarize(total_transfer_fee = sum(total_transfer_fee), .groups = "drop")   

edges <- transfers_tmp_df_season %>% 
  select(
    from = league,
    to = team_name
  )

nodes <- transfers_tmp_df_season %>% 
  select(node = team_name, league, size = total_transfer_fee) %>% 
  bind_rows(
    data.frame(
      node = unique(transfers_tmp_df_season$league),
      league = unique(transfers_tmp_df_season$league),
      size = 0)
  )

graph <- graph_from_data_frame(edges, vertices = nodes)

set.seed(1)
# ggraph(graph, layout = "circlepack", weight = size) + 
ggraph(graph) + 
  geom_node_circle(
    aes(fill = league, col = league), 
    size = 0.2, color = "white") +
  geom_node_label(
    aes(label = name),
    family = "Roboto Condensed", fontface = "bold", repel = TRUE,
    fill = alpha("grey12", 0.3), color = "grey94", label.size = 0
  ) 
