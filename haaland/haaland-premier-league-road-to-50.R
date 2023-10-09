library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)
library(lubridate)

base_path <- "haaland"

season_end_years <- 1993:2024

haaland_url <- "https://fbref.com/en/players/1f44ac21/Erling-Haaland"

quickest_top10_urls <- 
  c(
    "https://fbref.com/en/players/b6638a2c/Andy-Cole",
    "https://fbref.com/en/players/438b3a51/Alan-Shearer",
    "https://fbref.com/en/players/80a30989/Ruud-van-Nistelrooy",
    "https://fbref.com/en/players/bdf7b97d/Fernando-Torres",
    "https://fbref.com/en/players/e342ad68/Mohamed-Salah",
    "https://fbref.com/en/players/d5dd5f1f/Pierre-Emerick-Aubameyang",
    "https://fbref.com/en/players/4d034881/Sergio-Aguero",
    "https://fbref.com/en/players/c0c5ee74/Thierry-Henry",
    "https://fbref.com/en/players/23d97ca1/Kevin-Phillips",
    "https://fbref.com/en/players/a190d597/Diego-Costa"
  )

notable_others_urls <- c(
  "https://fbref.com/en/players/945dea33/Didier-Drogba",
  "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo",
  "https://fbref.com/en/players/21a66f6a/Harry-Kane",
  "https://fbref.com/en/players/88b3f52c/Michael-Owen",
  "https://fbref.com/en/players/a6154613/Luis-Suarez",
  "https://fbref.com/en/players/f07be45a/Wayne-Rooney",
  "https://fbref.com/en/players/3fcab3a8/Robin-van-Persie"
  )

# player_urls <- c(
#   quickest_top10_urls,
#   notable_others_urls
# )
player_urls <- c(quickest_top10_urls, 
                 "https://fbref.com/en/players/21a66f6a/Harry-Kane",
                 "https://fbref.com/en/players/a6154613/Luis-Suarez"
                 )
player_names <- str_extract(player_urls, "/([-a-zA-Z]+)$", group = 1) %>% 
  str_replace_all("-", " ")

if (FALSE) {
  goal_logs <- map(
    player_urls, 
    fb_player_goal_logs,
    goals_or_assists = "goals"
  )
  goal_logs <- set_names(goal_logs, player_urls)
  write_rds(goal_logs, here(base_path, "pl-quickest-goal-logs.rds"))
} else {
  goal_logs <- read_rds(here(base_path, "pl-quickest-goal-logs.rds"))
}

# Haaland
goal_logs_haaland <- fb_player_goal_logs(haaland_url, goals_or_assists = "goals")
goal_logs_haaland$Player_Url <- haaland_url

# get needed timespan for match logs
player_timespan <- bind_rows(goal_logs, .id = "Player_Url") %>% 
  bind_rows(goal_logs_haaland) %>% 
  filter(Comp == "Premier League") %>% 
  arrange(Player_Url, Date, Minute) %>%
  select(Player_Url, Date, Minute) %>% 
  group_by(Player_Url) %>% 
  mutate(Rk = row_number()) %>% 
  filter(Rk <= 50) %>% 
  mutate(year = year(Date)) %>% 
  summarize(min_year = min(year),
            max_year = max(year)) %>% 
  ungroup() %>% 
  # add one year
  mutate(max_year = max_year + 1)


player_years <- player_timespan %>% 
  crossing(season_end_year = season_end_years) %>% 
  filter(season_end_year >= min_year, season_end_year <= max_year) %>% 
  select(-c(min_year, max_year))

fb_player_match_logs_possibly <- possibly(fb_player_match_logs)

if (FALSE) {
  match_logs <- map2(
    player_years$Player_Url[player_years$Player_Url != haaland_url], 
    player_years$season_end_year[player_years$Player_Url != haaland_url], 
    fb_player_match_logs_possibly,
    stat_type = "summary"
  )
  write_rds(match_logs, here(base_path, "pl-quickest-match-logs.rds"))
} else {
  match_logs <- read_rds(here(base_path, "pl-quickest-match-logs.rds"))
}
 
# Retrieve match logs for Haaland in PL 
match_logs_haaland <- map2(
  player_years$Player_Url[player_years$Player_Url == haaland_url], 
  player_years$season_end_year[player_years$Player_Url == haaland_url], 
  fb_player_match_logs_possibly,
  stat_type = "summary"
)

# Prepare match logs, keep only PL, add cumulative goals
match_logs_prep <- match_logs %>% 
  c(match_logs_haaland) %>% 
  compact() %>% 
  bind_rows() %>% 
  filter(Comp == "Premier League") %>% 
  filter(Pos != "On matchday squad, but did not play") %>% 
  select(Player, Season, Date, goals = Gls_Performance) %>% 
  arrange(Player, Season, Date) %>% 
  distinct() %>% 
  group_by(Player) %>% 
  mutate(
    match_id = row_number(),
    goals_cumul = cumsum(goals)) %>% 
  ungroup() 

# Match logs Haaland - should be retrieved separately
match_logs_prep_haaland <- match_logs_prep %>% 
  filter(Player == "Erling Haaland")

unique(match_logs_prep$Player)

match_logs_prep %>% 
  group_by(Player) %>% 
  summarize(total_goals = sum(goals, na.rm = TRUE),
            max_goals_cumul = max(goals_cumul)) %>% 
  arrange(-total_goals)

# In case a player has no record for 50 cumulative goals because he scored multiple
# goals in the match (e.g. has 51/52 goals), select the match_id with the smallest
# amount of goals > 50
matches_needed_to_score_50 <- match_logs_prep %>% 
  filter(goals_cumul >= 50) %>% 
  group_by(Player) %>% 
  arrange(match_id, .by_group = TRUE) %>% 
  summarize(match_id = first(match_id)) %>% 
  arrange(match_id)

unique(matches_needed_to_score_50$Player)

match_logs_prep_haaland_last_match <- match_logs_prep_haaland %>% 
  tail(1) %>% 
  mutate(Player_facet = factor(
    Player, levels = c("Erling Haaland", matches_needed_to_score_50$Player)))

haaland_color <- "#4BC07E" #"#18dc82"

match_logs_prep %>% 
  inner_join(matches_needed_to_score_50, 
             by = join_by(Player, match_id <= match_id),
             suffix = c("", ".y")) %>% 
  # only keep players who needed 100 matches or less
  filter(match_id.y < 86 | Player == "Harry Kane") %>% 
  # select(-match_id.y) %>% 
  bind_rows(match_logs_prep_haaland) %>% 
  ggplot(aes(match_id, goals_cumul, group = Player)) +
  geom_step(
    col = "grey50", linewidth = 0.2
  ) +
  geom_step(
    data = ~mutate(., Player_facet = factor(
      Player, levels = c("Erling Haaland", matches_needed_to_score_50$Player))),
    aes(col = Player_facet == "Erling Haaland"),
    linewidth = 0.8
  ) +
  geom_point(
    data = match_logs_prep_haaland_last_match,
    size = 2, col = haaland_color
  ) +
  geom_text(
    data = match_logs_prep_haaland_last_match,
    aes(label = goals_cumul),
    vjust = -0.33, hjust = -0.33, col = haaland_color, size = 3,
    family = "Cabinet Grotesk", fontface = "bold"
  ) +
  # annotate(
  #   "segment",
  #   x = match_logs_prep_haaland_last_match$match_id,
  #   xend = match_logs_prep_haaland_last_match$match_id,
  #   y = 0, yend = match_logs_prep_haaland_last_match$goals_cumul,
  #   col = "#64113F", linetype = "dashed"
  # ) +
  # geom_segment(
  #   data = match_logs_prep_haaland_last_match,
  #   aes(
  #     x = match_id, xend = match_id,
  #     y = 0, yend = goals_cumul
  #   ),
  #   col = "#64113F", linetype = "dashed"
  # ) +
  # geom_segment(
  #   data = match_logs_prep_haaland_last_match,
  #   aes(
  #     x = 0, xend = match_id,
  #     y = goals_cumul, yend = goals_cumul
  #   ),
  #   col = "#64113F", linetype = "dashed"
  # ) +
  scale_color_manual(values = c("TRUE" = haaland_color, "FALSE" = "#7678ED")) +
  facet_wrap(vars(Player_facet)) +
  coord_cartesian(clip = "off") +
  guides(color = "none") + 
  labs(
    title = glue::glue("<span style='color:{haaland_color}'>Erling Haaland</span> to become the
    fastest player to score 50 Premier League goals"),
    subtitle = " Haaland has scored 44 goals in 41 appearances (1.07 goals per match).
    The current record holder is Andy Cole who needed 65 matches to 
    reach 50 Premier League goals.
    Haaland would still beat Cole's record if he needed 23 matches to score 6 more goals
    (0.26 goals per match - a quarter of his currrent goal scoring rate).",
    caption = "Data: FBRef.com. Visualization: Ansgar Wolsing",
    x = "Matches played",
    y = "Goals scored (cumulative)"
  ) + 
  theme_minimal(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = "grey97", fill = "grey97"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(lineheight = 1),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_textbox(width = 0.95),
    plot.title.position = "plot"
  )
ggsave(here(base_path, "haaland-pl-fastest-to-score-50-goals-20231008.png"),
       width = 8, height = 7)



## how many goals had the other played scored after x matches? 
match_logs_prep %>% 
  filter(match_id == 41) %>% 
  select(Player, goals_cumul) %>% 
  arrange(-goals_cumul)
