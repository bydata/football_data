library(tidyverse)
library(rvest)
library(tictoc)


# https://www.transfermarkt.de/1-bundesliga/startseite/wettbewerb/L1/plus/?saison_id=2018

scrape_transfermarkt_season <- function(year) {
  url <- sprintf("https://www.transfermarkt.de/1-bundesliga/startseite/wettbewerb/L1/plus/?saison_id=%s", year)
  page <- read_html(url)
  
  # extract result table using xpath and transform into data frame
  table <- html_node(page, xpath = "//table[@class='items']")
  
  # prepare table
  df <- html_table(table, fill = TRUE, header = FALSE) %>% 
    select(team = 2, n_players = 4, market_value = 7) %>% 
    slice(3:20) %>% 
    mutate(market_value = str_replace(market_value, "(Mio\\.|Tsd\\.)\\s€", "") %>% 
                str_replace(",", ".") %>% 
                as.numeric,
           n_players = as.numeric(n_players),
           avg_market_value = market_value / n_players)
    
    
  
  
  df
}

# scrape final tables all seasons at once and store them in a list (takes a while)
season_years <- 2005:2019
tic()
market_values <- map(season_years, scrape_transfermarkt_season)
toc()

# name the list items with season names
market_values <- market_values %>% 
  set_names(season_years)

str(market_values)

write_rds(market_values, "output/bundesliga_market_values.RData", compress = "gz")

# save as csv file
write_csv(bind_rows(market_values), "output/bundesliga_market_values.csv")



### all matchdays
library(parallel)

# scrape all seasons at once and store them in a list (takes a while)
tic()

seasons_mapping[seasons_mapping$season == "2019/2020", ]$matchday_id <- 34

seasons_matchday_mapping <- expand.grid(season = pull(seasons_mapping, season), matchday_id = 1:34) %>% 
  left_join(seasons_mapping, by = "season") %>% 
  filter(matchday_id.x <= as.numeric(matchday_id.y)) %>% 
  rename(matchday_id = matchday_id.x) %>% 
  select(season, season_id, matchday_id) %>% 
  arrange(season, matchday_id) 

scrape_table_possibly <- possibly(scrape_table, otherwise = NULL)


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, as.list(unique(c(ls(.GlobalEnv),ls(environment())))),envir = environment())
clusterEvalQ(cl,
             {library(tidyverse)
               library(rvest)
             })
all_tables <- clusterMap(cl, scrape_table_possibly, 
                         seasons_matchday_mapping$season, seasons_matchday_mapping$season_id, seasons_matchday_mapping$matchday_id)

stopCluster(cl)

#all_tables <- pmap(season_matchday_mapping[1:10, ], scrape_table_possibly)
toc()

# check if there are any missing matchdays
all_tables %>% 
  map(class) %>% 
  bind_cols() %>% 
  pivot_longer(cols = everything(), names_to = "season", values_to = "class") %>% 
  filter(class == "NULL")

#scrape_table("1995/1996", seasons_mapping[seasons_mapping$season == "1995/1996", "season_id"], 12)

write_rds(all_tables, "output/bundesliga_all_tables_list.RData")


all_tables %>% 
  bind_rows() %>% 
  distinct(team) %>% 
  arrange(team)

all_tables %>% 
  bind_rows() %>% 
  mutate(row = row_number()) %>% 
  filter(is.na(team))

all_tables %>% 
  bind_rows() %>% 
  mutate(row = row_number()) %>% 
  filter(row >= 1610 & row <= 1620)

table_6667_11 <- scrape_table("1966/1967", seasons_mapping[seasons_mapping$season == "1966/1967", "season_id"], 3)


# some cleaning of team names required
all_tables <- all_tables %>% 
  map(~mutate(.x,
              team = case_when(
                team == "Werder Bremen" ~ "SV Werder Bremen",
                str_detect(team, "Uerdingen") ~ "KFC Uerdingen 05",
                str_detect(team, "Hoffenheim") ~ "TSG Hoffenheim",
                team == "Meidericher SV" ~ "MSV Duisburg",
                TRUE ~ team
                )
  ))


all_tables_df <- bind_rows(all_tables)
write_rds(all_tables, "output/bundesliga_all_tables_list.RData")
write_rds(all_tables_df, "output/bundesliga_all_tables_df.RData")
write_csv(all_tables_df, "output/bundesliga_all_tables.csv")


all_tables %>% 
  map_df(class) %>% 
  pivot_longer(cols = everything(), names_to = "season", values_to = "class") %>% 
  distinct(class)


