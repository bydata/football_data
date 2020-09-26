library(tidyverse)
library(rvest)
library(tictoc)


# Scrape page IDs for season final tables from drop-down selection
# The value of the options contains the season id and matchday id (last matchday of season) combined

url <- "https://www.dfb.de/bundesliga/spieltagtabelle/"
page <- read_html(url)
ddoptions <- html_nodes(page, xpath = "//select[@id='seasons']/option")

seasons_mapping <- tibble(
  season = html_text(ddoptions),
  id = html_attr(ddoptions, name = "value")
) %>% 
  separate(id, into = c("season_id", "matchday_id"), sep = "\\|") %>% 
  arrange(season)


# use season mapping to send request to retrieve each seasons final table
# example url: https://www.dfb.de/bundesliga/spieltagtabelle/?spieledb_path=%2Fcompetitions%2F12%2Fseasons%2F17820%2Fmatchday%2F34

scrape_table <- function(season, season_id, matchday_id) {
  url <- sprintf("https://www.dfb.de/bundesliga/spieltagtabelle/?spieledb_path=%%2Fcompetitions%%2F12%%2Fseasons%%2F%s%%2Fmatchday%%2F%s", season_id, matchday_id)
  page <- read_html(url)
  
  # extract result table using xpath and transform into data frame
  result_table <- html_node(page, xpath = "//table[@class='data-table table-bordered']")
  
  # prepare table
  df <- html_table(result_table, fill = TRUE, header = TRUE)
  colnames(df) <- c("position", "X1", "team", "played", "won", "draw", "lost", "goals", "goal_diff", "points")
  df <- df %>% 
    select(-starts_with("X")) %>% 
    separate(goals, into = c("goals", "goals_against"), sep = ":") %>% 
    mutate(points_2pt = 2 * won + draw,
           points_3pt = 3 * won + draw,
           season = season,
           start_year = str_split_fixed(season, "/", 2)[, 2],
           start_year = as.numeric(start_year)) %>% 
    select(season, start_year, everything())
  df
}

# scrape final tables all seasons at once and store them in a list (takes a while)
tic()
final_tables <- pmap(seasons_mapping, scrape_table)
toc()

# name the list items with season names
final_tables <- final_tables %>% 
  set_names(seasons_mapping$season)

# remove current season from the list
final_tables[[nrow(seasons_mapping)]] <- NULL

write_rds(final_tables, "output/bundesliga_final_tables.RData", compress = "gz")

# save as csv file
final_tables_flat <- bind_rows(final_tables)
write_csv(final_tables_flat, "output/bundesliga_final_tables.csv")



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

#table_6667_11 <- scrape_table("1966/1967", seasons_mapping[seasons_mapping$season == "1966/1967", "season_id"], 3)


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


# all_tables %>% 
#   map_df(class) %>% 
#   pivot_longer(cols = everything(), names_to = "season", values_to = "class") %>% 
#   distinct(class)


