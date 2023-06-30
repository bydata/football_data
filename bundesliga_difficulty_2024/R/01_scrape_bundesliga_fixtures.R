library(tidyverse)
library(lubridate)
library(rvest)
library(glue)
library(tictoc)


## Scrape all fixtures from dfb.de -------------------------------------

# scrape fixtures for a given matchday from dfb.de
scrape_fixtures <- function(matchday) {
  base_url <- glue("https://www.dfb.de/bundesliga/spieltagtabelle/?spieledb_path=%2Fde%2Fcompetitions%2Fbundesliga%2Fseasons%2F2023-24%2Fmatchday%2F{matchday}")
  
  # regex to extract the date from the date column
  # (extracts the first occurence)
  date_regex <- "[0-3][0-9]\\.[0-1][0-9]\\.20\\d{2}"
  
  # retrieve page content, parse table with fixtures
  page <- read_html(base_url)
  table <- html_node(page, css = "table.table-match-comparison")
  df <- html_table(table)
  # transform table
  df <- 
    df %>% 
    select(1, 2, 6) %>% 
    rename(date_text = 1, home = 2, away = 3) %>% 
    as_tibble() %>% 
    mutate(matchday = matchday,
           date = str_extract(date_text, date_regex),
           date = dmy(date),
           year_month = floor_date(date, "month")) %>% 
    select(matchday, year_month, date, home, away, date_text)
  df
}

# safe function to handle errors
scrape_fixtures_possibly <- possibly(scrape_fixtures, otherwise = NULL)

# scrape matchdays 1 to 34
tic()
fixtures <- map_df(1:34, scrape_fixtures_possibly)
toc()

dim(fixtures)

# save output
write_rds(fixtures, "bundesliga_difficulty_2024/input/bundesliga_fixtures.rds")
write_csv(fixtures, "bundesliga_difficulty_2024/input/bundesliga_fixtures.csv")

