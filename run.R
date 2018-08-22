
library(tidyverse)

# scrape results - only run once and then comment out to save runtime
#source("scraping.R")

# provides a tibble "clubnames"
source("clubnames.R")

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

