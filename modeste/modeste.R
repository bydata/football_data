library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)

base_path <- "modeste"

current_season_players_standard <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "standard", team_or_player = "player")
write_rds(current_season_players_standard, here(base_path, "current_season_players_standard.rds"))

current_season_players_possession <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "possession", team_or_player = "player")
write_rds(current_season_players_possession, here(base_path, "current_season_players_possession.rds"))

min_played_threshold <- 4
buli_fw_standard <- current_season_players_standard %>% 
  filter(Comp == "Bundesliga", Pos == "FW") %>% 
  # filter(Comp == "Bundesliga", Pos %in% c("FW", "FW,MF")) %>% 
  filter(MP_Playing >= min_played_threshold)
  # filter(Min_Playing >= 90)

buli_fw_possession <- current_season_players_possession %>% 
  # filter(Comp == "Bundesliga", Pos %in% c("FW", "FW,MF")) 
  filter(Comp == "Bundesliga", Pos == "FW")

selected_players <- c("Anthony Modeste", "Youssoufa Moukoko", "Karim Adeyemi", "Donyell Malen")
buli_fw_standard %>% 
  inner_join(buli_fw_possession, by = c("Player", "Season_End_Year", "Squad", "Comp", "Pos")) %>% 
  tibble() %>% 
  select(Season_End_Year, Comp, Squad, Player, Pos, MP_Playing, Min_Playing, Gls, Ast, 
         Gls_Per, Ast_Per,  `Att 3rd_Touches`, Targ_Receiving, npxG_Per, `npxG+xA_Per`) %>% 
  mutate(Att_3rd_Touches_Per = `Att 3rd_Touches` / Min_Playing * 90,
         Targ_Receiving_Per = Targ_Receiving / Min_Playing * 90) %>% 
  ggplot(aes(Targ_Receiving_Per, npxG_Per)) +
  geom_point(aes(fill = Player %in% selected_players, color = Player %in% selected_players,
                 size = Min_Playing), 
             shape = 21) +
  ggrepel::geom_text_repel(
    data = ~subset(., Player %in% selected_players),
    aes(label = Player), size = 2, family = "Roboto Condensed", fontface = "bold") +
  scale_fill_manual(values = c("FALSE" = "grey68", "TRUE" = "#FDE100")) +
  scale_color_manual(values = c("FALSE" = "white", "TRUE" = "grey8")) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Modeste yet to make an impact at Borussia Dortmund",
    subtitle = sprintf(
    "Non-penalty expected goals per 90 mins by the number of times a pass
      was attempted to this player.
      Bundesliga forwards with at least %s matches played 
      in the 2022/2023 season (matchday 8)
    ", min_played_threshold),
    caption = "Source: FBRef. Visualisation: Ansgar Wolsing",
    x = "Target receiving per 90 mins",
    y = "npxG per 90 mins",
    size = "Total minutes played"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    text = element_text(color = "grey24", lineheight = 1.1),
    axis.text = element_text(color = "grey24"),
    plot.title = element_text(color = "grey2", face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.92)
  )
ggsave(here(base_path, "modeste_target-receiving_vs_thread-2.png"), width = 6, height = 6)
