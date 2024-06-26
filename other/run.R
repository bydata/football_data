
library(tidyverse)
library(parallel)


# scrape results - only run once and then comment out to save runtime
source("scraping.R")

# get all results from 1963/1981 onwards - use parallelisation
system.time( {
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, as.list(unique(c(ls(.GlobalEnv),ls(environment())))),envir=environment())
  clusterEvalQ(cl,
               {library(tidyverse)
                 library(rvest)
                 }
               )
  pokal <- parLapply(cl, map_chr(1981:2017, format_year), scrape_season) %>%
    data.table::rbindlist() %>%
    as_tibble()
  
  stopCluster(cl)
  cl <- NULL
})

saveRDS(pokal, "dfbpokal_allseasons.RData")
#count_stages <- pokal %>% count(season, stage)

# get leagues tables (to be parallelized)
system.time({
bundesliga1_df <- map2("1. Bundesliga", 1981:2017, scrape_league_table) %>%
  data.table::rbindlist()

bundesliga2_df <- map2("2. Bundesliga", 1981:2017, scrape_league_table) %>%
  data.table::rbindlist()
})

saveRDS(bundesliga1_df, "bundesliga1_df.RData")
saveRDS(bundesliga2_df, "bundesliga2_df.RData")

# provides a tibble "clubnames"
clubnames <- read_tsv("clubnames.tsv")

pokal_clean <- pokal %>%
  left_join(clubnames, by = c("home" = "club_raw"))  %>% 
  left_join(clubnames, by = c("away" = "club_raw"))  %>% 
  mutate(home = ifelse(!is.na(club_clean.x), club_clean.x, home),
         away = ifelse(!is.na(club_clean.y), club_clean.y, away)) %>%
  select(-club_clean.x, -club_clean.y) %>%
  # Leipzig has to be cleaned based on the year since there are 3+1 clubs named Leipzig (VfB, RB, S. = Sachsen, VfB II)
  # Rules:
  ## Leipzig until 2004: VfB Leipzig
  ## Leipzig from 2011: RB Leipzig
  mutate(home = ifelse(home == "Leipzig", 
                       ifelse(season >= "2011-12", "RB Leipzig", "VfB Leipzig"),
                       home
                  ),
    away = ifelse(away == "Leipzig", 
                  ifelse(season >= "2011-12", "RB Leipzig", "VfB Leipzig"),
                  away
            ),
    home = ifelse(home == "Uerdingen", 
                  ifelse(season >= "1995-96", "KFC Uerdingen 05", "Bayer 05 Uerdingen"),
                  home
    ),
    away = ifelse(away == "Uerdingen", 
                  ifelse(season >= "1995-96", "KFC Uerdingen 05", "Bayer 05 Uerdingen"),
                  away
    ),
    home = ifelse(home == "Remscheid", 
                   ifelse(season >= "1990-91", "FC Remscheid", "BVL Remscheid"),
                   home
    ),
    away = ifelse(away == "Remscheid", 
                  ifelse(season >= "1990-91", "FC Remscheid", "BVL Remscheid"),
                  away
    )
  ) 

clubs <- pokal_clean %>%
  select(home, away) %>%
  gather(key = key, value = club, home, away) %>%
  select(club) %>%
  distinct() %>%
  arrange(club)

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
         bl2_started_earlier = (season >= "2011-12"),
         won = ifelse((home_away == "home" & winner == 1) | (home_away == "away" & winner == 2), TRUE, FALSE)
         ) %>%
  select(match_key, season, stage, club, league, opponent_league, home_away, won, bl2_started_earlier) %>%
  arrange(match_key, home_away)


# check if join and mapping of clubnames was successful
# for all seasons - check missing matches between league tables and cup teams
bundesliga1_df %>%
  select(season, club) %>%
  anti_join(pokal_league_tidy, by = c("club", "season")) %>%
  distinct()

bundesliga2_df %>%
  select(season, club, preseason) %>%
  anti_join(pokal_league_tidy, by = c("club", "season")) %>%
  distinct()

bundesliga2_df %>%
  select(season, club, preseason) %>%
  anti_join(pokal_league_tidy, by = c("club", "season")) %>%
  filter(is.na(preseason)) %>%
  distinct()


# how many seasons had 7 stages?
pokal_league_tidy %>%
  distinct(season, stage) %>%
  count(stage)
# which seasons had 7 stages?
(season_to_remove <- pokal_league_tidy %>%
    filter(stage == 7) %>%
    distinct(season)
)

# remove 5 seasons with 7 stages
pokal_league_tidy_2 <- pokal_league_tidy %>%
  anti_join(season_to_remove, by = "season")

str(pokal_league_tidy_2)


# EXPLORATION

pokal_league_tidy_2 %>%
  filter(league == "1. Bundesliga" & opponent_league == "2. Bundesliga") %>%
  group_by(bl2_started_earlier, stage, league) %>%
  summarize(win_share = mean(won),
            total = n()
            ) %>%
  arrange(stage, bl2_started_earlier) %>%
  ggplot(aes(stage, win_share, fill = bl2_started_earlier)) +
  geom_col(position = "dodge")



pokal_league_tidy_2 %>%
  filter(league == "1. Bundesliga" & opponent_league == "2. Bundesliga" & stage == 1)

pokal_league_tidy_2 %>%
  filter(league == "1. Bundesliga" & opponent_league == "2. Bundesliga" & stage == 6)

pokal_league_tidy_2 %>%
  filter(league == "1. Bundesliga" & opponent_league == "2. Bundesliga") %>%
  count(stage)


logodds2prob <- function(x) {
  odds <- exp(x)
  prob <- odds / (1 + odds)
  prob
}




pokal_1blvs2bl <- pokal_league_tidy_2 %>%
  filter(league == "1. Bundesliga" & opponent_league == "2. Bundesliga") %>%
  mutate(stage1 = stage == 1)

win_model <- glm(won ~ stage, pokal_1bl, family = binomial)
summary(win_model)

logodds2prob(win_model$coefficients)


win_model_2 <- glm(won ~ bl2_started_earlier*stage1, pokal_1blvs2bl, family = binomial)
summary(win_model_2)

logodds2prob(win_model_2$coefficients)
