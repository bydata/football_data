library(tidyverse)
library(parallel)
library(EloRating)

source("scraping.R")

scrape_bundesliga_results <- function(year) {
  scrape_season_results("1. Bundesliga", year)
}

# get all final standings from from 1963 onwards - use parallelisation
system.time( {
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, as.list(unique(c(ls(.GlobalEnv),ls(environment())))),envir=environment())
  clusterEvalQ(cl,
               {library(tidyverse)
                 library(rvest)
               }
  )
  bundesliga_df <- parLapply(cl, 1963:2017, scrape_bundesliga_results) %>%
    data.table::rbindlist() %>%
    as_tibble() %>%
    arrange(season, match_day, home_team, away_team)
  stopCluster(cl)
  cl <- NULL
})


# generate an artificial date value for match days
create_date <- function(season, match_day) {
  start_date <- lubridate::ymd(str_c(str_sub(season, 1, 4), 8, 25, sep = "-"))   # set a start date in August
  days_from_start <- (match_day - 1) * 7  # assume 7 days between each match day
  match_date <- start_date + lubridate::days(days_from_start)
  match_date
} 

# determine season from date
get_season <- function(date) {
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  season <- ifelse(month < 8,
                   str_c((year - 1), str_sub(year, 3, 4), sep = "-"),
                   str_c(year, str_sub((year + 1), 3, 4), sep = "-")
                   )
  season
}


# prepare dataset for ELO calculation
matches_eloprep <- bundesliga_df %>%
  mutate(
    date = create_date(season, match_day),
    result_3way = factor(ifelse(home_goals > away_goals, "home",
                                ifelse(away_goals > home_goals, "away", "draw"))),
    winner = ifelse(result_3way == "home", home_team, away_team), # in case of a draw, winner and loser are interchangable
    loser  = ifelse(result_3way == "home", away_team, home_team),
    draw   = (result_3way == "draw")
  )


seqcheck(winner = matches_eloprep$winner, loser = matches_eloprep$loser, draw = matches_eloprep$draw, Date = matches_eloprep$date)

res <- elo.seq(winner = matches_eloprep$winner, loser = matches_eloprep$loser, draw = matches_eloprep$draw, Date = matches_eloprep$date, k = 24)

summary(res)

eloplot(res, ids = c("Borussia Dortmund", "Bayern München"), from = "1963-08-01", to = "2018-07-01")
eloplot(res, ids = c("Borussia Dortmund", "Bayern München", "Hamburger SV"))


all_elo <- as_tibble(res$mat) %>%
  cbind(date = res$truedates, .) %>%
  gather(key = team, value = elo, -date) %>%
  filter(!is.na(elo)) %>%
  mutate(season = get_season(date),
         team = ifelse(team == "1. FC Dynamo Dresden", "Dynamo Dresden", team))  # adapt Dynamo Dresden team name

#############################################

# get leagues tables (to be parallelized)
system.time({
  bundesliga_tables <- map2("1. Bundesliga", 1963:2017, scrape_league_table) %>%
    data.table::rbindlist()
})

# add final standings to the elo ratings
all_elo_finaltable <- all_elo %>%
  inner_join(bundesliga_tables, by = c("team" = "club", "season")) %>%
  select(season, date, team, elo, seed, W, L, D, goals_scored, goals_against, goal_diff) %>%
  mutate(points = 3 * W + D) %>% # calculate points based on 3 points for a win 
  arrange(season, seed, date)
  
## check potential missing values
all_elo %>%
  anti_join(bundesliga_tables, by = c("team" = "club", "season")) %>%
  distinct(team)

## check club names
bundesliga_tables %>%
  distinct(club, season) %>%
  filter(str_detect(club, "Offenbach|Dresden"))

#############################################


team_list <- c("Bayern München", "Bor. Mönchengladbach", "Borussia Dortmund", "Hamburger SV")
team_colors <- c("red", "green", "#ffcc00", "blue")
all_elo %>%
  filter(team %in% team_list) %>%
  ggplot(aes(date, elo, col = team)) +
  geom_step() +
  geom_hline(yintercept = 1000) +
  scale_color_manual(values = team_colors) +
  ggthemes::theme_fivethirtyeight()
ggsave("elo_1.png")


all_elo %>%
  filter(team %in% team_list) %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(team, year) %>%
  summarize_at("elo", funs(mean, max, min)) %>%
  ggplot(aes(year, mean, col = team)) + 
  geom_step(size = 1) +
  geom_ribbon(aes(year, ymin = min, ymax = max, fill = team), alpha = 0.05) +
  geom_hline(yintercept = 1000) +
  scale_color_manual(values = team_colors) +
  scale_fill_manual(values = team_colors) +
  ggthemes::theme_fivethirtyeight()


all_elo %>%
  filter(team %in% team_list) %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(team, year) %>%
  summarize_at("elo", funs(mean, max, min)) %>%
  ggplot(aes(year, mean, col = team)) + 
  geom_step() +
  geom_hline(yintercept = 1000) +
  scale_color_manual(values = team_colors) +
  scale_fill_manual(values = team_colors) +
  ggthemes::theme_fivethirtyeight()
ggsave("elo_2.png")

## compare ELO rating of champion and runner- (TODO: correct the wrong champions)

champions_runnerup <- bundesliga_tables %>%
  filter(seed <= 2) %>%
  mutate(points = 3 * W + D,
         year = as.numeric(str_sub(season, 1, 4)) + 1) %>%
  select(year, seed, club, points) %>%
  group_by(year) %>%
  summarize(point_diff = max(points) - min(points),
            champion = first(club)) %>%
  ungroup()
  
elo_diff_per_year <- all_elo_finaltable %>%
  mutate(year = as.numeric(str_sub(season, 1, 4)) + 1) %>%
  group_by(year, team, seed, points, goal_diff) %>%
  filter(seed <= 2) %>%
  summarize(mean_elo = mean(elo)) %>%
  spread(seed, mean_elo, sep = "_") %>%
  mutate(champion = ifelse(!is.na(seed_1), team, NA)) %>%
  group_by(year) %>%
  summarize(seed_1 = max(seed_1, na.rm = TRUE),
            seed_2 = max(seed_2, na.rm = TRUE),
            elo_diff_1v2_abs = seed_1 - seed_2,
            elo_diff_1v2_rel = elo_diff_1v2_abs / seed_1,
            point_diff = max(points) - min(points),
            champion = max(champion, na.rm = TRUE)) %>% 
  ungroup()
  
# check if champions were identified correctly (3-points per win vs. 2 points per win)
elo_diff_per_year %>%
  inner_join(champions_runnerup, by = "year") %>%
  filter(champion.x != champion.y)

team_colors <- c("darkred", "red", "black", "lightskyblue", "red2", "lawngreen", "#ffcc00", "mediumpurple4", "blue", "brown1", "darkolivegreen4", "forestgreen")
elo_diff_per_year %>%  
  ggplot(aes(year, elo_diff_1v2_abs, group = 1)) +
  geom_line(size = 0.1) +
  geom_point(aes(size = point_diff, col = champion)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(aes(yintercept = max(elo_diff_1v2_abs[champion != "Bayern München"])), col = "grey", linetype = 2) + 
  geom_label(aes(x = 2016, y = max(elo_diff_1v2_abs[champion != "Bayern München"])), 
             label = "Max. difference of ELO ratings with other club \n winning the league title (M'gladbach 1974/'75)", size = 2.5) +
  geom_rect(aes(xmin = 2013.3, xmax = 2018.7, ymin = 120, ymax = 280), fill = NA, col = "grey") + 
  scale_color_manual(values = team_colors) +
  labs(title = "Absolute differences in ELO ratings between champion and runner-up",
       col = "Champion", size = "Points differential") +
  ggthemes::theme_fivethirtyeight()
ggsave("elo_3.png")

elo_diff_per_year %>%  
  ggplot(aes(year, elo_diff_1v2_abs, group = 1, fill = champion)) +
  geom_col() +
  #geom_point(aes(size = point_diff, col = champion)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(aes(yintercept = max(elo_diff_1v2_abs[champion != "Bayern München"])), col = "grey", linetype = 2) + 
  #geom_rect(aes(xmin = 2013.3, xmax = 2018.7, ymin = 120, ymax = 280), fill = NA, col = "grey") + 
  scale_fill_manual(values = team_colors) +
  labs(title = "Absolute differences in ELO ratings between champion and runner-up",
       fill = "Champion", size = "Points differential") +
  ggthemes::theme_fivethirtyeight()
ggsave("elo_4.png")



###### Calculate ELO ratings by season #####

n_seasons <- (2017-1963+1)
res_by_year <- vector("list", n_seasons)
for (i in 1:n_seasons) {
  matches <- matches_eloprep %>%
    mutate(year = as.numeric(str_sub(season, 1, 4)) + 1) %>%
    filter(year == 1963 + i)
  res_by_year[i] <- elo.seq(winner = matches$winner, loser = matches$loser, draw = matches$draw, Date = matches$date, k = 24)
}

str(res_by_year)

