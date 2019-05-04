library(tidyverse)
library(parallel)

source("scraping.R")

scrape_bundesliga_results <- function(year) {
  scrape_season_results("1. Bundesliga", year)
}

# get all match results from from 1963 onwards
system.time( {
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, as.list(unique(c(ls(.GlobalEnv),ls(environment())))),envir=environment())
  clusterEvalQ(cl,
               {library(tidyverse)
                 library(rvest)
               }
  )
  bundesliga_df <- parLapply(cl, 1963:2018, scrape_bundesliga_results) %>%
    data.table::rbindlist() %>%
    as_tibble() %>%
    arrange(season, home_team, away_team)
  stopCluster(cl)
  cl <- NULL
})

results <- bundesliga_df %>%
  mutate(goal_diff = abs(home_goals - away_goals),
         goal_sum = home_goals + away_goals,
         year = as.numeric(str_sub(season, 1, 4))) 

season_stats <- results %>%
  group_by(year) %>%
  summarize(goals_per_game = mean(goal_sum),
            goal_diff_4 = sum(goal_diff >= 4) / n(),
            goal_diff_5 = sum(goal_diff >= 5) / n(),
            goal_diff_6 = sum(goal_diff >= 6) / n(),
            goal_sum_6  = sum(goal_sum >= 6) / n(),
            goal_sum_7  = sum(goal_sum >= 7) / n())


# calculated the number of unique teams which scored 6 or more goals in a single match
no_teams_per_season_6goals <- results %>%
  gather(key = home_away, value = team, home_team, away_team) %>%
  mutate(goals = ifelse(home_away == "home_team", home_goals, away_goals)) %>%
  select(season, year, team, goals) %>%
  filter(goals >= 6) %>%
  distinct(season, year, team) %>%
  group_by(season, year) %>%
  summarize(no_teams_6goals = n())

# check if 11 teams with 6 or more goals scored in a single match correct (it actually is)
results %>%
  filter(year == 1965) %>%
  gather(home_away, team, home_team, away_team) %>%
  mutate(goals = ifelse(home_away == "home_team", home_goals, away_goals)) %>%
  filter(goals >= 6) %>%
  distinct(team)


# merge the summaries
season_stats <- season_stats %>%
  inner_join(no_teams_per_season_6goals)


season_stats %>%
  ggplot(aes(year, goal_diff_6)) +
  geom_line() +
  geom_point(aes(size = goals_per_game)) + 
  ggtitle("Share of matches with 6 or more goals difference") +
  labs(x = "Year", y = "Share of matches", size = "Average goals per game") + 
  scale_y_continuous(labels = scales::percent)


results %>%
  ggplot(aes(year, goal_diff)) +
  geom_hex(bins = c(12, 56))

results %>%
  ggplot(aes(goal_sum, goal_diff, color = year)) +
  geom_jitter(alpha = 0.5)


season_stats %>%
  ggplot(aes(no_teams_6goals, goal_diff_6)) +
  geom_label(aes(label = year, fill = year), size = 3+1, col = "white", alpha = 0.75) +
  annotate("text", x = 4.7, y = 0.039, label = "After just 8 match days") + 
  ggtitle("Share of matches with 6 or more goals difference") +
  labs(y = "Share of matches", x = "Number of teams which scored 6 or more goals in a single match", fill = "Year") +
  scale_x_continuous(breaks = seq(1, 11, 1)) +
  scale_y_continuous(labels = scales::percent)

# randomly pick 8 matches from each team ...
set.seed(1909)
