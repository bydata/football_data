library(tidyverse)
library(parallel)

source("scraping.R")

# get results from current Bundesliga season
bl2018 <- scrape_season_results("1. Bundesliga", 2018)

# create long dataset with Eintracht Frankfurt results
sge <- "Eintracht Frankfurt"
sge_season_goals <- 39
sge_matches_played <- 19
 
sge_results <- bl2018 %>%
  filter(home_team == sge | away_team == sge) %>%
  mutate(opponent = ifelse(home_team == sge, away_team, home_team)) %>%
  select(-season, result) %>%
  gather(key = home_away, value = team, -home_goals, -away_goals, -opponent) %>%
  filter(team == sge) %>%
  mutate(
    sge_points = case_when(
      home_goals == away_goals  ~ 1,
      home_goals > away_goals & home_away == "home_team" ~ 3,
      away_goals > home_goals & home_away == "away_team" ~ 3,
      TRUE ~ 0
    ),
    sge_goals = case_when(
      home_away == "home_team" ~ home_goals,
      home_away == "away_team" ~ away_goals
    )
  ) %>%
  arrange(opponent)

# manually enter current league standings
standings <- as_tibble(
    list(position = seq(1, 18),
         team = c("Borussia Dortmund",
                  "Bayern München",
                  "Bor. Mönchengladbach",
                  "RB Leipzig",
                  "Eintracht Frankfurt",
                  "TSG Hoffenheim",
                  "Hertha BSC",
                  "VfL Wolfsburg",
                  "Bayer 04 Leverkusen",
                  "1. FSV Mainz 05",
                  "Werder Bremen",
                  "FC Schalke 04",
                  "SC Freiburg",
                  "Fortuna Düsseldorf",
                  "FC Augsburg",
                  "VfB Stuttgart",
                  "Hannover 96",
                  "1. FC Nürnberg"
                  ),
         color = c("yellow",
                   "red",
                   "green",
                   "red",
                   "red",
                   "lightblue",
                   "blue",
                   "lightgreen",
                   "red",
                   "red",
                   "green",
                   "blue",
                   "red",
                   "red",
                   "white",
                   "red",
                   "red",
                   "red"
                   )
          )
      )

standings

sge_results <- sge_results %>%
  inner_join(standings, by = c("opponent" = "team")) %>%
  select(-home_goals, -away_goals, -team, -home_away, opponent_position = position) %>%
  group_by(opponent, color) %>%
  summarize(
    opponent_position = max(opponent_position),
    sge_points = sum(sge_points),
    sge_goals  = sum(sge_goals),
    n = n()
  ) %>%
  ungroup() %>%
  arrange(opponent_position) %>%
  mutate(sge_points_cumul = cumsum(sge_points),
         max_points = lag(sge_points_cumul) + n * 3,
         max_points = ifelse(is.na(max_points), 0, max_points),
         sge_goals_cumul  = cumsum(sge_goals),
         #match_count = cumsum(n),
         sge_goals_uniform = cumsum(n) * sge_season_goals/sge_matches_played
         ) 



# check: cumulative points in last row must equal to total points (TRUE)


sge_results %>%
  arrange(opponent_position) %>%
  ggplot(aes(opponent_position)) +
  geom_col(aes(y = max_points), fill = "white") +
  geom_col(aes(y = sge_points_cumul), fill = "#DC143C") +
  ggthemes::theme_fivethirtyeight() +
  labs(title = "Eintracht erfolgreich gegen Teams aus der unteren Tabellenhälfte",
       x = "Opponent position",
       y = "Points") +
  geom_text(aes(opponent_position, y = 15, label = opponent, angle = 90, hjust = 0))



sge_results %>%
  bind_rows(list(opponent = "---", sge_points = NA, sge_goals = NA, opponent_position = 5, n = NA, color = "red", sge_points_cumul = sge_results$sge_points_cumul[4], max_points = sge_results$max_points[4], sge_goals_uniform = sge_results$sge_goals_uniform[4])) %>%
  arrange(opponent_position) %>%
  ggplot(aes(opponent_position)) +
  geom_area(aes(y = sge_goals_cumul), fill = "#DC143C") +
  geom_line(aes(y = sge_goals_uniform), linetype = "dashed") +
  ggthemes::theme_fivethirtyeight() +
  labs(title = "Eintracht erfolgreich gegen Teams aus der unteren Tabellenhälfte",
       x = "Opponent position",
       y = "Points") +
  geom_text(aes(opponent_position, y = 25, label = opponent, angle = 90, hjust = 0), col = "#555555")


sge_results %>%
  mutate(level = factor(ceiling(opponent_position / 6))) %>%
  group_by(level) %>%
  summarize(sge_goals_mean = mean(sge_goals),
            sge_goals_sd   = sd(sge_goals))

