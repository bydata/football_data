library(tidyverse)
library(ggtext)
library(worldfootballR)
library(grid)
library(here)

base_path <- "fcb-worst-opponents"

season_end_years <- 2015:2024
df <- load_match_results(country = "GER", gender = "M", tier = "1st",
                         season_end_year = season_end_years)
colnames(df) <- tolower(colnames(df))
colnames(df)

fcb <- "Bayern Munich"
threshold_matches <- 4

results_against_fcb <- df %>% 
  select(season_end_year, wk, home, away, homegoals, awaygoals) %>% 
  filter(home == fcb | away == fcb) %>%
  # remove the matches that haven't been played in the current season
  filter(!is.na(homegoals)) %>% 
  mutate(
    home_goaldiff = homegoals - awaygoals,
    opponent_goaldiff = ifelse(home == fcb, -home_goaldiff, home_goaldiff), 
    opponent_result = case_when(
      opponent_goaldiff >  0 ~ "Win",
      opponent_goaldiff == 0 ~ "Draw",
      opponent_goaldiff <  0 ~"Loss"
    ),
    winner = case_when(
      home_goaldiff >  0 ~ "Home",
      home_goaldiff == 0 ~ "Draw",
      home_goaldiff <  0 ~"Away"
    ),
    winner_fcb = case_when(
      home == fcb & winner == "Home" ~ "FCB",
      away == fcb & winner == "Away" ~ "FCB",
      home != fcb & winner == "Home" ~ "Opponent",
      away != fcb & winner == "Away" ~ "Opponent",
      winner == "Draw" ~ "Draw"
    )  
  ) %>%
  pivot_longer(cols = c(home, away), names_to = "home_away", values_to = "team") %>% 
  # keep only the away matches in Munich
  filter(home_away == "away") |> 
  filter(team != "Bayern Munich") %>% 
  select(-c(homegoals, awaygoals, home_goaldiff, winner, winner_fcb)) %>% 
  rename(goaldiff = opponent_goaldiff, result = opponent_result) %>% 
  mutate(
    wk = as.numeric(wk),
    home_away = factor(home_away, levels = c("home", "away")),
    result = factor(result, levels = c("Win", "Draw", "Loss"))) |> 
  # Keep teams with at least n matches
  group_by(team) |> 
  filter(n() >= threshold_matches) %>%
  ungroup() |> 
  # Add row for BVB win 2024-03-30 (TEMP)
  add_row(
    season_end_year = 2024, wk = 27, goaldiff = 2, result = "Win",
    home_away = "away", team = "Dortmund"
  ) 

teams_ordered_by_success <- results_against_fcb |> 
  group_by(team) |> 
  summarize(
    wins_n = sum(result == "Win"),
    draws_n = sum(result == "Draw"),
  ) |> 
  arrange(-wins_n, -draws_n) |> 
  pull(team)



ragg::agg_png(here(base_path, "fcb-opponents-results.png"),
       width = 4, height = 5, scaling = 0.8, res = 300, units = "in")
results_against_fcb |> 
  mutate(
    team = factor(team, levels = rev(teams_ordered_by_success)),
    result = factor(result, levels = c("Win", "Draw", "Loss"),
                    labels = c("Sieg", "Unentschieden", "Niederlage"))) |> 
  arrange(team, result) |>
  mutate(row = row_number(), .by = team) |> 
  ggplot(aes(team, row)) +
  geom_point(
    aes(col = result),
    shape = 15, size = 5) + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1),
    breaks = seq(0, 10, 2),
    expand = expansion(add = c(0.5, 3))) +
  scale_color_manual(values = c("#FDE100", "grey30", "grey90")) +
  coord_flip() +
  labs(
    title = "Wer Punkte in München holte",
    subtitle = "Anzahl der <span style='font-family:\"Outfit SemiBold\"'>Siege</span>,
    <span style='color:grey30;font-family:\"Outfit SemiBold\"'>Unentschieden</span> und
    <span style='font-family:\"Outfit SemiBold\"'>Niederlagen</span> in Auswärtsspielen
    gegen Bayern München in der Bundesliga<br>(Stand 27. Spieltag 2023/'24)",
    caption = "Zeitraum: Saison 2014/'15 bis 2023/'24 (einschließlich 27. Spieltag). 
    Dargestellt sind Mannschaften, die mindestens 4 Spiele in München bestritten haben.<br>
    Daten: FBRef.com. Visualisierung: Ansgar Wolsing",
    col = NULL
  ) +
  theme_minimal(base_family = "Outfit Light") +
  theme(
    plot.background = element_rect(color = "#FDFDFD", fill = "#FDFDFD"),
    plot.margin = margin(t = 4, b = 2, l = 12, r = 12),
    panel.spacing.x = unit(1, "cm"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.key.height = unit(2, "mm"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y.left = element_markdown(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(hjust = 0),
    plot.title = element_text(family = "Outfit Semibold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.99, hjust = 0, lineheight = 1.25, margin = margin(t = 4, b = 10)),
    plot.caption = element_textbox(
      width = 0.99, hjust = 0, size = 7, lineheight = 1.1),
    plot.caption.position = "plot"
  )

grid.lines(
  x = c(0.185, 0.26), y = 0.918, gp = gpar(col = "#FDE100", lwd = 4)
)
grid.lines(
  x = c(0.28, 0.48), y = 0.918, gp = gpar(col = "grey30", lwd = 4)
)
grid.lines(
  x = c(0.545, 0.715), y = 0.918, gp = gpar(col = "grey90", lwd = 4)
)
grid.rect(
  x = 0.42, y = 0.475, width = 0.8, height = 0.04, 
  gp = gpar(col = "black", fill = "transparent", lwd = 1)
)
dev.off()
