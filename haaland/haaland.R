library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)

base_path <- "haaland"

current_season_players_standard <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "standard", team_or_player = "player")
write_rds(current_season_players_standard, here(base_path, "current_season_players_standard.rds"))

current_season_players_possession <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "possession", team_or_player = "player")
write_rds(current_season_players_possession, here(base_path, "current_season_players_possession.rds"))

min_played_threshold <- 4
big5_fw_standard <- current_season_players_standard %>% 
  filter(Pos %in% c("FW", "FW,MF")) %>% 
  filter(MP_Playing >= min_played_threshold & Min_Playing >= 180)


selected_players <- c("Erling Haaland", "Lionel Messi", "Kylian Mbappé", "Robert Lewandowski", 
                      "Cristiano Ronaldo", "Harry Kane", "Leroy Sané",
                      "Sadio Mané", "Anthony Modeste", "Rayan Cherki", "Darwin Núñez", 
                      "Ansu Fati", "Beto", "Thomas Müller", 
                      "Callum Wilson", "Gabriel Jesus", "Roberto Firmino")
big5_fw_standard_poss <- big5_fw_standard %>% 
  inner_join(current_season_players_possession, by = c("Player", "Season_End_Year", "Squad", "Comp", "Pos")) %>% 
  tibble() %>% 
  select(Season_End_Year, Comp, Squad, Player, Pos, MP_Playing, Min_Playing, Gls, Ast, 
         Gls_Per, Ast_Per,  `Att 3rd_Touches`, Targ_Receiving, npxG_Per, `npxG+xA_Per`) %>% 
  mutate(Att_3rd_Touches_Per = `Att 3rd_Touches` / Min_Playing * 90,
         Targ_Receiving_Per = Targ_Receiving / Min_Playing * 90)

big5_fw_standard_poss %>% 
  ggplot(aes(npxG_Per, Gls_Per)) +
  geom_point(aes(
    fill = case_when(
      Player == "Erling Haaland" ~ Player,
      Comp == "Premier League" ~ "Premier League",
      TRUE ~ "Other")), 
      color = "white", shape = 21, size = 2.5) +
  ggrepel::geom_text_repel(
    data = ~subset(., Player %in% selected_players),
    aes(label = Player, color = Player == "Erling Haaland"), size = 2, family = "Roboto Condensed", fontface = "bold") +
  scale_fill_manual(values = c("Other" = "grey68", "Erling Haaland" = "#D1495B", "Premier League" = "#30638E")) +
  scale_color_manual(values = c("FALSE" = "grey24", "TRUE" = "#D1495B")) +
  guides(fill = "none", color = "none") +
  labs(
    title = "<span style='color:#D1495B'>Erling Haaland</span> well ahead of 
    <span style='color:#3D5A80'>Premier League strikers</span>",
    subtitle = sprintf(
    "Non-penalty expected goals per 90 mins vs Goals per 90 mins.
      Forwards in Top 5 leagues with at least %s matches and total 180 minutes played 
      in the 2022/2023 season
    ", min_played_threshold),
    caption = "Source: FBRef. Visualisation: Ansgar Wolsing",
    #x = "npxG per 90 mins",
    # y = "Goals per 90 mins",
    size = "Total minutes played"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    text = element_text(color = "grey24", lineheight = 1.1),
    axis.text = element_text(color = "grey24"),
    plot.title = element_markdown(color = "grey2", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.92)
  )
ggsave(here(base_path, "haaland_goals-xG.png"), width = 6, height = 6)
