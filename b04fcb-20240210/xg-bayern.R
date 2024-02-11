library(tidyverse)
library(ggtext)
library(worldfootballR)

base_path <- "b04fcb-20240210"

# Retrieve the results for the last seasons
season_end_years <- 2018:2023
results_fbref_previous_seasons <- load_match_results(country = "GER", gender = "M", tier = "1st",
                      season_end_year = season_end_years)
results_fbref_current_season <- fb_match_results(country = "GER", gender = "M", tier = "1st",
                                                   season_end_year = 2024)
results_fbref_combined <- bind_rows(results_fbref_previous_seasons, results_fbref_current_season) %>% 
  janitor::clean_names() %>% 
  mutate(wk = as.numeric(wk))

results_fbref_long <- results_fbref_combined %>% 
  mutate(
    fixture = paste(home, away, sep = " - ")
  ) %>% 
  select(season_end_year, wk, fixture, home_team = home, away_team = away, home_goals, away_goals, 
         home_xg = home_x_g, away_xg = away_x_g) %>% 
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") %>% 
  mutate(
    xG = ifelse(home_away == "home_team", home_xg, away_xg),
    goals = ifelse(home_away == "home_team", home_goals, away_goals)) %>% 
  select(-matches("home_|away_")) 

xg_fcb_b04fcb <- results_fbref_long$xG[
  results_fbref_long$team == "Bayern Munich" & 
    results_fbref_long$season_end_year == 2024 &
    results_fbref_long$wk == 21]

results_fbref_long %>% 
  filter(team == "Bayern Munich") %>% 
  nrow()

results_fbref_long %>% 
  filter(team == "Bayern Munich") %>% 
  filter(xG <= xg_fcb_b04fcb)

results_fbref_long %>% 
  filter(team == "Bayern Munich") %>% 
  filter(xG < xg_fcb_b04fcb) %>% 
  count(season_end_year)

2 / 238

results_fbref_long %>% 
  filter(team == "Bayern Munich") %>% 
  filter(xG > 5) %>% 
  count(season_end_year)

results_fbref_long %>% 
  filter(xG > 5) %>% 
  nrow()

results_fbref_long %>% 
  filter(team == "Bayern Munich") %>% 
  ggplot(aes(xG)) +
  geom_histogram(
    aes(fill = xG <= xg_fcb_b04fcb),
    binwidth = 0.4, linewidth = 0.4, color = "white", show.legend = FALSE) +
  annotate(
    "text",
    x = c(0, 5), y = 12.5,
    label = c("Lower or\nequal xG", "Higher xG"),
    color = c("#EE3838", "grey60"),
    family = "Quicksand SemiBold", hjust = 0.33, size = 5, 
    lineheight = 0.9
  ) + 
  annotate(
    GeomCurve,
    x = 0.4, xend = 0.35, y = 10, yend = 4.25,
    color = "#EE3838", curvature = 0.1, linewidth = 0.2,
    arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed")
  ) +
  # scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("grey72", "#EE3838")) +
  coord_cartesian(clip = "off") +
  labs(
    title = sprintf(
      "FC Bayern had %0.1f expected goals against Bayer Leverkusen.
      In the last 7 seasons, they had a 
      <span style='color:#EE3838'>lower or equal xG value</span> in only 3 other matches", 
      xg_fcb_b04fcb),
    caption = "Data: FBRef.com (2017/18 to 2023/24, matchday 21). Visualization: Ansgar Wolsing",
    x = "xG", y = "Matches"
  ) +
  theme_minimal(base_family = "Quicksand") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_textbox(
      family = "Quicksand SemiBold", width = 0.98, lineheight = 1.2),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 1),
    axis.title.y = element_text(angle = 90, hjust = 0.5, vjust = 1)
  )
ggsave(file.path(base_path, "b04fcb-20240210-xG-histogram.png"),
       width = 5, height = 5, scale = 1.2)
