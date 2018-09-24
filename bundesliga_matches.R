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
    arrange(season, home_team, away_team)
  stopCluster(cl)
  cl <- NULL
})


# prepare dataset for ELO calculation
matches_eloprep <- bundesliga_df %>%
  mutate(
    date = as.Date(ifelse(season == "1999-00", "2000-06-30",
                  str_c(str_sub(season, 1, 2), str_sub(season, 6, 7), "-06-30"))),
    result_3way = factor(ifelse(home_goals > away_goals, "home",
                                ifelse(away_goals > home_goals, "away", "draw"))),
    winner = ifelse(result_3way == "home", home_team, away_team), # in case of a draw, winner and loser are interchangable
    loser  = ifelse(result_3way == "home", away_team, home_team),
    draw   = (result_3way == "draw")
  )


seqcheck(winner = matches_eloprep$winner, loser = matches_eloprep$loser, draw = matches_eloprep$draw, Date = matches_eloprep$date)

res <- elo.seq(winner = matches_eloprep$winner, loser = matches_eloprep$loser, draw = matches_eloprep$draw, Date = matches_eloprep$date, runcheck=TRUE)

summary(res)
eloplot(res)

eloplot(res, ids = c("Borussia Dortmund", "Bayern München"), from = "1990-01-01", to = "2018-07-01")
eloplot(res, ids = c("Borussia Dortmund", "Bayern München", "Hamburger SV"))


all_elo <- as_tibble(res$mat) %>%
  filter(rowSums(is.na(.)) < length(colnames(.))) %>%
  cbind(year = c(1963:2017)) %>%
  select(year, everything())

all_elo_tidy <- all_elo %>%
  gather(key = team, value = elo, -year)

team_list <- c("Bayern München", "Borussia Dortmund", "Hamburger SV")
all_elo_tidy %>%
  filter(team %in% team_list) %>%
  ggplot(aes(year, elo, col = team)) +
  geom_line() +
  geom_hline(yintercept = 1000) +
  scale_color_manual(values = c("red", "#ffcc00", "blue")) +
  ggthemes::theme_fivethirtyeight()
ggsave("elo_1.png")


# calculate ELO ratings in 5-year brackets

