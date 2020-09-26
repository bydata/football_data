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


# use season mapping to send request to retrieve each seasons final result crosstable page
scrape_crosstable_pages <- function(season, season_id, matchday_id) {
  url <- sprintf("https://www.dfb.de/bundesliga/spieltagtabelle/?spieledb_path=%%2Fcompetitions%%2F12%%2Fseasons%%2F%s%%2Fmatchday%%2F%s", season_id, matchday_id)
  page <- read_html(url)
  page
}
 
extract_crosstable <- function(page) { 
  # extract crosstable using xpath and transform into data frame
  crosstable <- page %>% 
    html_node(xpath = "//div[@id='table-cross']/table") 
  
  df <- crosstable %>% 
    html_table(crosstable, fill = TRUE, header = TRUE)
  
  # column header
  columns <- html_nodes(crosstable, css = "thead th img") %>% html_attr("title")
  # row names
  rows <- html_nodes(crosstable, css = "tbody tr th img") %>% html_attr("title")
  
  # column names and row names to dataframe
  colnames(df) <- c("X1", columns)
  df <- df %>% 
    bind_cols(home = rows) %>% 
    select(home, everything(), -X1) %>% 
    # mutate_all(.funs = list(~str_remove_all(., fixed("\n")) %>% 
    #                           str_trim())) %>% 
    pivot_longer(cols = c(-home), names_to = "away", values_to = "result") %>% 
    mutate(result = str_remove_all(result, fixed("\n")) %>%
                      str_trim()) %>% 
    # exclude diagonal 
    filter(result != "") %>% 
    separate(result, into = c("home_goals", "away_goals"), sep = ":") %>% 
    mutate_at(vars(home_goals, away_goals), as.numeric) %>% 
    mutate(season = season) %>% 
    select(season, everything())
  
  df
}

# scrape crosstable pages for all seasons at once and store them in a list (takes a while)
tic()
pages <- pmap(seasons_mapping, scrape_crosstable_pages)
toc()

# extract crosstables from pages
crosstables <- pmap(list(pages), extract_crosstable)

# name the list items with season names
crosstables <- crosstables %>% 
  set_names(seasons_mapping$season)

write_rds(final_tables, "output/bundesliga_results_crosstable.RData", compress = "gz")

# save as csv file
crosstables_flat <- bind_rows(crosstables)
write_csv(crosstables_flat, "output/bundesliga_crosstables.csv")

