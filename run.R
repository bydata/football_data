
library(tidyverse)

# scrape results - only run once and then comment out to save runtime
source("scraping.R")

# get all results from 1963 onwards
pokal <- scrape_season(map_chr(1981:2017, format_year))
saveRDS(pokal, "dfbpokal_allseasons.RData")
#count_stages <- pokal %>% count(season, stage)

# get leagues tables

bundesliga1_df <- map2("1. Bundesliga", 1981:2017, scrape_league_table) %>%
  data.table::rbindlist()

bundesliga2_df <- map2("2. Bundesliga", 1981:2017, scrape_league_table) %>%
  data.table::rbindlist()

# provides a tibble "clubnames"
clubnames <- read_tsv("clubnames.tsv")

pokal_clean <- pokal %>%
  left_join(clubnames, by = c("home" = "club_raw"))  %>% 
  left_join(clubnames, by = c("away" = "club_raw"))  %>% 
  mutate(home = ifelse(!is.na(club_clean.x), club_clean.x, home),
         away = ifelse(!is.na(club_clean.y), club_clean.y, away)) %>%
  select(-club_clean.x, -club_clean.y)

clubs <- pokal_clean %>%
  select(home, away) %>%
  gather(key = key, value = club, home, away) %>%
  select(club) %>%
  distinct() %>%
  arrange(club)


leipzig <- pokal_clean %>%
  gather(key = key, value = club, home, away) %>%
  select(club, season, stage) %>%
  filter(str_detect(club, "Leipzig")) %>%
  arrange(club, season, stage)
# leipzig has to be cleaned based on the year since there are at least 2 clubs named Leipzig (VfB, RB)

# there is 1 NA case in home_goals and away_goals
pokal_clean %>% filter(is.na(home_goals)) # > match was decided in court: http://www.kicker.de/news/fussball/dfbpokal/startseite/287700/artikel_sportfreunde-siegen-am-gruenen-tisch.html
# cleanup
pokal_clean <- pokal_clean %>%
  mutate(home_goals = ifelse(is.na(home_goals), 2, home_goals),
         away_goals = ifelse(is.na(away_goals), 2, away_goals),
         winner = ifelse(is.na(away_goals), "away", winner)
         )

summary(pokal_clean)


### join cup and league tables
pokal_league <- pokal_clean %>%
  left_join(bundesliga1_df, by = c("home" = "club", "season")) %>%
  left_join(bundesliga1_df, by = c("away" = "club", "season")) %>%
  left_join(bundesliga2_df, by = c("home" = "club", "season")) %>%
  left_join(bundesliga2_df, by = c("away" = "club", "season")) %>%
  select(season:winner,
         home_league_bl1 = league.x,
         away_league_bl1 = league.y,
         home_league_bl2 = league.x.x,
         away_league_bl2 = league.y.y
  ) %>%
  mutate(
    home_league = ifelse(!is.na(home_league_bl1), home_league_bl1, 
                         ifelse(!is.na(home_league_bl2), home_league_bl2, "3. and below")),
    away_league = ifelse(!is.na(away_league_bl1), away_league_bl1, 
                         ifelse(!is.na(away_league_bl2), away_league_bl2, "3. and below")),
    leagues = factor(str_c(home_league, "vs.", away_league, sep = " "))
  ) %>%
  select(-contains("_league_bl")) %>%
  select(season, stage, home, away, winner, home_league, away_league, leagues) 


# tidy version of pokal tibble
pokal_league_tidy <- pokal_league %>%
  mutate(match_key = str_c(season, stage, home, away, sep = "#")) %>%
  gather(key = home_away, value = club, home, away) %>%
  mutate(home_away = factor(home_away, levels = c("home", "away")),
         league = ifelse(home_away == "home", home_league, away_league),
         opponent_league = ifelse(home_away == "home", away_league, home_league),
         won = ifelse((home_away == "home" & winner == 1) | (home_away == "away" & winner == 2), TRUE, FALSE)
         ) %>%
  select(match_key, season, stage, club, league, opponent_league, home_away, won) %>%
  arrange(match_key, home_away)


# exploration
pokal_league_tidy %>%
  filter(league == "1. Bundesliga" & opponent_league == "2. Bundesliga" | league == "2. Bundesliga" & opponent_league == "1. Bundesliga" ) %>%
  filter(league == "2. Bundesliga" & won)

pokal_league_tidy %>%
  filter(league == "2. Bundesliga" & opponent_league == "1. Bundesliga") %>%
  group_by(stage, league) %>%
  summarize(win_share = mean(won))

summary(pokal_league_tidy)
  
  