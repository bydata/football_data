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


# all_elo <- as_tibble(res$mat) %>%
#   filter(rowSums(is.na(.)) < length(colnames(.))) %>%
#   cbind(year = c(1963:2017)) %>%
#   select(year, everything())
# all_elo_tidy <- all_elo %>%
#   gather(key = team, value = elo, -year)


all_elo <- as_tibble(res$mat) %>%
  cbind(date = res$truedates, .) %>%
  gather(key = team, value = elo, -date) %>%
  filter(!is.na(elo))


team_list <- c("Bayern München", "Borussia Dortmund", "Hamburger SV")
all_elo %>%
  filter(team %in% team_list) %>%
  ggplot(aes(date, elo, col = team)) +
  geom_line() +
  geom_hline(yintercept = 1000) +
  scale_color_manual(values = c("red", "#ffcc00", "blue")) +
  ggthemes::theme_fivethirtyeight()
ggsave("elo_1.png")



