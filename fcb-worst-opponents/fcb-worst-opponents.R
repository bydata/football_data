library(tidyverse)
library(ggtext)
library(worldfootballR)
library(here)

base_path <- "fcb-worst-opponents"

season_end_years <- 2016:2024
df <- load_match_results(country = "GER", gender = "M", tier = "1st",
                         season_end_year = season_end_years)
colnames(df) <- tolower(colnames(df))
colnames(df)

fcb <- "Bayern Munich"
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
  filter(team != "Bayern Munich") %>% 
  select(-c(homegoals, awaygoals, home_goaldiff, winner, winner_fcb)) %>% 
  rename(goaldiff = opponent_goaldiff, result = opponent_result) %>% 
  # Add row for Bochum win 2024-02-18 (TEMP)
  add_row(
    season_end_year = 2024, wk = "22", goaldiff = 1, result = "Win",
    home_away = "home", team = "Bochum"
  ) %>% 
  mutate(
    wk = as.numeric(wk),
    home_away = factor(home_away, levels = c("home", "away")),
    result = factor(result, levels = c("Win", "Draw", "Loss")))

  
threshold_matches <- 6

results_against_fcb %>% 
  count(team, sort = TRUE) %>% 
  filter(n >= threshold_matches) %>% 
  nrow()

results_against_fcb %>% 
  group_by(team) %>% 
  mutate(
    n_matches = n(),
    total_goaldiff = sum(goaldiff),
    mean_goaldiff = mean(goaldiff),
    min_goaldiff = min(goaldiff)) %>% 
  ungroup() %>% 
  filter(n_matches >= threshold_matches) %>% 
  count(team, goaldiff, min_goaldiff, home_away, name = "n_matches") %>% 
  mutate(team = fct_reorder(team, min_goaldiff)) %>% 
  ggplot(aes(team, goaldiff)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "steelblue") +
  geom_point(aes(size = n_matches)) + 
  scale_y_continuous(breaks = seq(-10, 10, 2)) +
  coord_flip() +
  facet_wrap(vars(home_away))


results_against_fcb %>% 
  group_by(team) %>% 
  mutate(
    n_matches = n(),
    total_goaldiff = sum(goaldiff),
    mean_goaldiff = mean(goaldiff),
    min_goaldiff = min(goaldiff)) %>% 
  ungroup() %>% 
  filter(n_matches >= threshold_matches) %>% 
  ggplot(aes(team, goaldiff)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "steelblue") +
  # geom_point(
  #   position = position_jitter(width = 0.3, height = 0)) + 
  ggbeeswarm::geom_beeswarm() +
  scale_y_continuous(breaks = seq(-10, 10, 2)) +
  coord_flip() +
  facet_wrap(vars(home_away))



results_against_fcb_with_aggregates <- results_against_fcb %>% 
  group_by(team) %>% 
  mutate(
    n_matches = n(),
    n_wins = sum(result == "Win"),
    n_losses = sum(result == "Loss"),
    n_home_matches = sum(home_away == "home"),
    n_away_matches = sum(home_away == "away"),
    n_home_losses = sum(result == "Loss" & home_away == "home"),
    n_away_losses = sum(result == "Loss" & home_away == "away"),
    n_home_draws = sum(result == "Draw" & home_away == "home"),
    n_away_draws = sum(result == "Draw" & home_away == "away"),
    share_wins = n_wins / n_matches,
    share_losses = n_losses / n_matches,
    share_home_losses = n_home_losses / n_home_matches,
    share_away_losses = n_away_losses / n_away_matches,
    share_home_draws = n_home_draws / n_home_matches,
    share_away_draws = n_away_draws / n_away_matches,
    share_away_notwon = share_away_losses + share_away_draws
  ) %>% 
  ungroup() %>% 
  filter(n_matches >= threshold_matches) %>%
  mutate(
    team = fct_reorder(team, -share_away_losses),
    team_label = sprintf("%s (%d)", team, n_matches),
    team_label = fct_reorder2(team_label, -share_away_notwon, -share_away_losses),
    home_away = factor(home_away, levels = c("away", "home")))

results_against_fcb_with_aggregates %>% 
  ggplot(aes(team_label, fill = fct_rev(result))) + 
  geom_bar(position = "fill") +
  geom_hline(aes(yintercept = 0.5), color = "grey40") +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  colorspace::scale_fill_discrete_sequential() +
  coord_flip() +
  facet_wrap(vars(home_away), ncol = 2) +
  labs(
    x = NULL,
    fill = NULL
  ) +
  theme_light() +
  theme(
    panel.spacing.x = unit(1, "cm")
  )


# with total matches as 3rd facet
results_against_fcb_with_aggregates %>% 
  bind_rows(
    mutate(results_against_fcb_with_aggregates, home_away = "total")
  ) %>% 
  mutate(
    result = c("Win" = "Sieg", "Draw" = "Unentschieden", "Loss" = "Niederlage")[result],
    result = factor(result, levels = c("Sieg", "Unentschieden", "Niederlage")),
    # team_label = if_else(team == "Dortmund", sprintf("**%s**", team_label), team_label),
    team_label = fct_reorder(team_label, -share_losses),
    home_away = factor(home_away, levels = c("total", "home", "away"),
                       labels = c("Alle Spiele", "Heimspiele", "In München"))
    ) %>% 
  ggplot(aes(team_label, fill = fct_rev(result))) + 
  geom_bar(position = "fill") +
  geom_hline(aes(yintercept = 0.5), color = "grey40", linewidth = 0.3) +
  scale_y_continuous(
    labels = scales::label_percent(), expand = c(0, 0),
    minor_breaks = seq(0, 1, 0.1)
    ) +
  colorspace::scale_fill_discrete_sequential(palette = "Reds 2") +
  coord_flip() +
  facet_wrap(vars(home_away), ncol = 3) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Angstgegner Mönchengladbach",
    subtitle = "Ergebnisse gegen **Bayern München** in Bundesliga-Spielen 
    seit der Saison 2015/'16.<br>(In Klammern die Anzahl der Begegnungen.)",
    caption = "Ausgewählt sind alle Vereine, die im angegebenen Zeitraum mindestens
    sechs Spiele gegen Bayern München bestritten haben (Stand: 18.02.2024).<br>
    Daten: FBRef.com. Visualisierung: Ansgar Wolsing.",
    x = NULL,
    y = "Anteil Spiele (%)",
    fill = NULL
  ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "#FDFDFD", fill = "#FDFDFD"),
    plot.margin = margin(t = 4, b = 2, l = 12, r = 12),
    panel.spacing.x = unit(1, "cm"),
    legend.position = c(-0.22, 1.1),
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.key.height = unit(2, "mm"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y.left = element_markdown(),
    plot.title = element_text(family = "Outfit Semibold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.95, hjust = 0, lineheight = 1.1, margin = margin(t = 4, b = 24)),
    plot.caption = element_textbox(
      width = 0.95, hjust = 0, size = 7, lineheight = 1),
    plot.caption.position = "plot"
  )
ggsave(here(base_path, "fcb-worst-opponents.png"),
       width = 4, height = 3, scale = 1.83)




# reordered per home_away facet
# TODO: Restructure data to make tidytext::reorder_within work
# with total matches as 3rd facet
results_against_fcb_with_aggregates %>% 
  bind_rows(
    mutate(results_against_fcb_with_aggregates, home_away = "total")
  ) %>% 
  # distinct(team, team_label, result, home_away, n_matches, share_losses) %>% 
  mutate(
    team_label = fct_reorder(team_label, -share_losses),
    home_away = factor(home_away, levels = c("total", "home", "away"),
                       labels = c("Alle Spiele", "Heimspiele", "In München")),
    team_label = tidytext::reorder_within(team_label, by = -share_losses, within = home_away)
  ) %>% View()
  ggplot(aes(team_label, fill = fct_rev(result))) + 
  geom_bar(position = "fill") +
  geom_hline(aes(yintercept = 0.5), color = "grey40", size = 0.5) +
  tidytext::scale_x_reordered() +
  scale_y_continuous(
    labels = scales::label_percent(), expand = c(0, 0),
    minor_breaks = seq(0, 1, 0.1)
  ) +
  colorspace::scale_fill_discrete_sequential() +
  coord_flip() +
  facet_wrap(vars(home_away), ncol = 3, scales = "free_y") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(1, "cm"),
    # panel.ontop = TRUE,
    legend.position = "top",
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )




## Team season points ==========================================================

team_season_points <- df %>% 
  select(season_end_year, wk, home, away, homegoals, awaygoals) %>% 
  # remove the matches that haven't been played in the current season
  filter(!is.na(homegoals)) %>% 
  mutate(
    home_goaldiff = homegoals - awaygoals,
    winner = case_when(
      home_goaldiff >  0 ~ "home",
      home_goaldiff == 0 ~ "draw",
      home_goaldiff <  0 ~ "away"
    )
  ) %>% 
  pivot_longer(cols = c(home, away), names_to = "home_away", values_to = "team") %>% 
  mutate(
    result = case_when(
      home_away == "home" & winner == "home" ~ "win",
      home_away == "home" & winner == "away" ~ "loss",
      home_away == "away" & winner == "away" ~ "win",
      home_away == "away" & winner == "home" ~ "loss",
      winner == "draw" ~ "draw"
    ),
    points = case_when(
      result == "win" ~ 3,
      result == "draw" ~ 1,
      result == "loss" ~ 0
    )
  ) %>% 
  # exclude current (incomplete) season
  add_count(team, season_end_year, name = "n_matches") %>% 
  filter(n_matches == 34) %>% 
  group_by(team, season_end_year) %>% 
  summarize(season_total_points = sum(points), .groups = "drop") %>% 
  arrange(season_end_year, -season_total_points)


results_against_fcb %>% 
  inner_join(team_season_points, by = join_by(team, season_end_year)) %>% 
  ggplot(aes(season_total_points, goaldiff)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(
    aes(shape = home_away),
    alpha = 0.8,
    position = position_jitter(height = 0.2, width = 0.2)
  ) +
  scale_shape_manual(values = c(16, 21)) +
  theme_light()


results_against_fcb %>% 
  inner_join(team_season_points, by = join_by(team, season_end_year)) %>% 
  ggplot(aes(season_total_points, goaldiff)) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  geom_hex() +
  theme_light()

# grouped season total points
results_against_fcb %>% 
  inner_join(team_season_points, by = join_by(team, season_end_year)) %>% 
  mutate(season_total_points_grp = case_when(
    season_total_points >= 60 ~ "60+ points",
    season_total_points >= 40 ~ "40-59 points",
    TRUE ~ "<40 points"
  )) %>% 
  count(season_total_points_grp, goaldiff, name = "n_matches") %>% 
  group_by(season_total_points_grp) %>% 
  mutate(share_matches = n_matches / sum(n_matches)) %>% 
  ungroup %>% 
  complete(season_total_points_grp, goaldiff, 
           fill = list(n_matches = 0, share_matches = 0)) %>% 
  ggplot(aes(season_total_points_grp, goaldiff)) +
  geom_tile(
    aes(fill = share_matches),
    color = NA) +
  geom_point(
    data = results_against_fcb %>% 
      inner_join(team_season_points, by = join_by(team, season_end_year)) %>% 
      mutate(season_total_points_grp = case_when(
        season_total_points >= 60 ~ "60+ points",
        season_total_points >= 40 ~ "40-59 points",
        TRUE ~ "<40 points"
      )) %>% 
      filter(team == "Dortmund"),
    aes(shape = home_away),
    position = position_jitter(width = 0.1, height = 0.2, seed = 1),
    color = "grey8", size = 2.5
  ) +
  geom_hline(aes(yintercept = 0), color = "red") +
  scale_x_discrete(position = "top") +
  scale_shape_manual(values = c(16, 21)) +
  colorspace::scale_fill_continuous_sequential() +
  coord_cartesian(expand = FALSE) +
  # guides(
  #   shape = guide_legend(override.aes = list(color = "grey50"))
  # ) +
  labs(
    x = NULL
  ) +
  theme_light(base_family = "Outfit")


# grouped season total points
results_against_fcb %>% 
  inner_join(team_season_points, by = join_by(team, season_end_year)) %>% 
  mutate(season_total_points_grp = case_when(
    season_total_points >= 60 ~ "60+ points",
    season_total_points >= 40 ~ "40-59 points",
    TRUE ~ "<40 points"
  )) %>% 
  ggplot(aes(season_total_points_grp, goaldiff)) +
  geom_hline(aes(yintercept = 0), color = "red") +
  ggbeeswarm::geom_quasirandom(
    aes(shape = home_away,
        col = team == "Dortmund"),
    size = 2.5
  ) +
  scale_x_discrete(position = "top") +
  scale_color_manual(values = c("FALSE" = "grey55", "TRUE" = "purple")) +
  scale_shape_manual(values = c(16, 21)) +
  coord_cartesian(expand = FALSE) +
  # guides(
  #   shape = guide_legend(override.aes = list(color = "grey50"))
  # ) +
  labs(
    x = NULL
  ) +
  theme_light(base_family = "Outfit") +
  theme(
    legend.position = "bottom"
  )
