library(tidyverse)
library(rvest)
library(here)

base_path <- "nationalmannschaft"

base_url <- "https://www.dfb.de/maenner-nationalmannschaft/spiele-termine/?spieledb_path=%2Fteams%2Fdeutschland%2Fseasonplan%2F"
season_start_year <- 1907:2023
season_end_year <- season_start_year + 1
seasons <- paste(season_start_year, str_sub(season_end_year, 3, 4), sep = "-")
urls <- paste0(base_url, seasons)


get_table <- function(url) {
  page <- read_html(url)
  raw_table <- page %>% 
    html_node(css = "table") %>% 
    html_table()
  raw_table
}

extract_data <- function(x) {
  x %>% 
    select(match_info = 1, home_team = 2, result = 4, away_team = 6) %>% 
    mutate(
      match_time = str_extract(match_info, "(\\d{2}:\\d{2}) Uhr", group = 1),
      match_date = str_extract(match_info, "\\d{2}\\.\\d{2}\\.\\d{4}"),
      match_date = dmy(match_date),
      competition = str_extract(match_info, "(.+)\\\n", group = 1),
      result_extra = str_extract(result, "\\d+\\s:\\s\\d+([^\\d]+)", group = 1),
      result = str_extract(result, "(\\d+\\s:\\s\\d+)")) %>% 
    separate(result, into = c("home_goals", "away_goals"), sep = " : ", 
             convert = TRUE, remove = FALSE) %>% 
    arrange(match_date, match_time) %>% 
    select(match_date, match_time, competition, home_team, away_team, 
           home_goals, away_goals, result_extra)
}

# extract_data(raw_tables[[31]])


raw_tables <- map(urls, get_table)
write_rds(raw_tables, here(base_path, "data", "raw_tables.rds"))

results <- raw_tables %>% 
  keep(function(x) nrow(x) > 0) %>% 
  map_dfr(extract_data) %>% 
  mutate(match_id = row_number()) %>% 
  select(match_id, everything())

# results2 <- raw_tables_2 %>% 
#   keep(function(x) nrow(x) > 0) %>% 
#   map_dfr(extract_data) %>% 
#   mutate(match_id = row_number()) %>% 
#   select(match_id, everything())
# 
# results <- bind_rows(results, results2) %>% 
#   distinct()

