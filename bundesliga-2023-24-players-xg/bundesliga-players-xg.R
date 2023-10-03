library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)
library(lubridate)

base_path <- "bundesliga-2023-24-players-xg"

# Retrieve shot locations (and xG)
shot_locations <- understat_league_season_shots(
  league = "Bundesliga",season_start_year = 2023)

shot_locations <- shot_locations %>% 
  mutate(player = ifelse(player == "Sehrou Guirassy", "Serhou Guirassy", player))

df_prep <- shot_locations %>% 
  # Remove penalties
  filter(situation != "Penalty") %>% 
  group_by(player) %>% 
  summarize(
    n_non_penalty_shots = n(),
    total_npxG = sum(xG),
    n_non_penalty_goals = sum(result == "Goal")
  ) %>% 
  arrange(-n_non_penalty_goals) %>% 
  filter(n_non_penalty_shots >= 5) 

# identify remarkable players
df_prep %>% 
  filter(n_non_penalty_goals > total_npxG * 1.5 & n_non_penalty_goals > 2)
df_prep %>% 
  filter(total_npxG > n_non_penalty_goals * 1.25 & n_non_penalty_goals > 0)

highlight_players <- df_prep %>% 
  filter(n_non_penalty_goals > total_npxG * 1.3 & n_non_penalty_goals > 2 |
           total_npxG > n_non_penalty_goals * 1.25 & n_non_penalty_goals > 1 |
           total_npxG > 2 & n_non_penalty_goals == 0 |
           player == "Harry Kane")

bg_color <- "grey97"

df_prep %>% 
  ggplot(aes(total_npxG, n_non_penalty_goals)) +
  geom_abline(
    aes(intercept = 0, slope = 1),
    color = "#E5446D", linetype = "dashed", size = 1
  ) +
  geom_point(
    data = ~subset(., player == "Serhou Guirassy"),
    shape = 21, size = 20, stroke = 0.8, color = "#E5446D"
  ) +
  geom_point(
    aes(fill = player %in% highlight_players$player),
    shape = 24, color = "grey20", size = 2, stroke = 0.2
  ) +
  ggrepel::geom_label_repel(
    data = ~filter(., player %in% highlight_players$player),
    aes(label = player),
    family = "Cabinet Grotesk", size = 2.5,
    min.segment.length = 0, segment.size = 0.2, seed = 1, fill = bg_color,
    label.size = 0, label.r = unit(0, "mm"), label.padding = unit(0, "mm")
  ) +
  annotate(
    "label",
    x = c(1, 6, 6),
    y = c(9, 1, 7),
    label = c("Mehr Tore als xG", "Weniger Tore als xG",
              "xG und erzielte Tore\nstimmen überein"),
    hjust = 0.5,
    family = "Cabinet Grotesk", size = 3, color = "grey40", fill = bg_color,
    label.size = c(0.1, 0.1, 0)
  ) +
  scale_y_continuous(breaks = seq(0, 40, 2)) +
  scale_fill_manual(values = c("FALSE" = "#F8F4E3", "TRUE" = "#E5446D")) +
  coord_cartesian(clip = "off") +
  guides(fill = "none") +
  labs(
    title = "Serhou Guirassy outperformt xG",
    subtitle = "Der Stürmer vom VfB Stuttgart kommt mit seinen 21 Torschüssen
    auf ca. 5 expected Goals, traf aber bereits 10 Mal (ohne Elfmeter)",
    caption = "Daten: Understat (Stand: 5. Spieltag). Visualisierung: Ansgar Wolsing",
    x = "Expected Goals (ohne Elfmeter)",
    y = "Erzielte Tore (ohne Elfmeter)"
  ) + 
  theme_minimal(base_family = "Cabinet Grotesk", base_size = 10) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.95, lineheight = 1),
    legend.position = "bottom"
  )
ggsave(here(base_path, "bundesliga-players-npxg-vs-goals-20230930.png"),
       width = 6, height = 5)

# Histogram of shots
shot_locations %>% 
  # Remove penalties
  filter(situation != "Penalty") %>% 
  count(player, name = "n_non_penalty_shots") %>% 
  ggplot(aes(n_non_penalty_shots)) +
  geom_histogram(color = "white", linewidth = 0.2, fill = "#E5446D") + 
  ggrepel::geom_text_repel(
    data = ~subset(., player %in% c("Harry Kane", "Sehrou Guirassy", "Victor Boniface")),
    aes(y = 1, label = sprintf("%s (%d)", player, n_non_penalty_shots)),
    family = "Cabinet Grotesk", min.segment.length = 0, segment.size = 0.2,
    nudge_y = 5, size = 3
  ) +
  labs(
    title = "Boniface schießt und schießt und schießt",
    subtitle = "Anzahl der Spieler mit einer bestimmten Anzahl von Torschüssen
    (ohne Elfmeter) in der Bundesliga zum 5. Spieltag",
    caption = "Daten: Understat (Stand: 5. Spieltag). Visualisierung: Ansgar Wolsing",
    x = "Torschüsse (ohne Elfmeter)",
    y = "Anzahl Spieler"
  ) + 
  theme_minimal(base_family = "Cabinet Grotesk", base_size = 10) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.95, lineheight = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave(here(base_path, "bundesliga-players-shots-histogram-20230930.png"),
       width = 6, height = 5)

