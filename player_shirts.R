library(tidyverse)
library(rvest)

source("scraping.R")


scrape_whoscored_players <- function(ws_player_id) {
  ws_player_id <- 95953
  url <- str_c("https://www.whoscored.com/Players/", ws_player_id)
  html_content <- get_content(url)

  player_info_blocks <- html_nodes(html_content, "dl") 
  info_text <- str_squish(str_trim(html_text(player_info_blocks)))
  
  player_info <- str_split_fixed(info_text, ": ", 2) %>%
    as_tibble() %>%
    spread(V1, V2) %>%
    select(Name, `Full Name`, `Age`, `Current Team`, everything())
  colnames(player_info) <- str_replace_all(colnames(player_info), " ", "_") %>% str_to_lower()
  
  league_info <- html_node(html_content, css = "#breadcrumb-nav") %>% 
    html_text() %>% str_squish() %>% str_split_fixed(" » ", 3) %>%
    as_tibble() %>%
    rename(league_country = V1, league_name = V2) %>%
    select(league_country, league_name)
  
  player_tbl <- cbind(player_info, league_info) %>%
    mutate(birthdate = lubridate::dmy(str_match(age, "(\\d{2}) years old \\((\\d{2}-\\d{2}-\\d{4})\\)")[3]),
           weight = as.numeric(str_remove(weight, "kg")),
           height = as.numeric(str_remove(height, "cm")),
           shirt_number = as.numeric(shirt_number)) %>%
    select(-age)
    
  player_tbl
}


#test <- map_dfr(1:100, scrape_whoscored_players)


scrape_whoscored_players(95953)
  
#https://www.whoscored.com/Players/95953/Show/Paco-Alc%C3%A1cer