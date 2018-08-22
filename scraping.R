
library(tidyverse)
library(rvest)


# retrieve page content - takes either a URL string or a vector of strings which constitute a url (will be collapsed to one string using "/")
get_content <- function(url) {
  if (is.vector(url)) {
    url <- str_c(url, collapse = "/")
  }
  url %>%
    read_html()
}


# url_vector <- c(base = "http://www.kicker.de/news/fussball/dfbpokal/spielrunde/dfb-pokal", 
#                 season = "2012-13",
#                 round = 2, # value between 1 (1st round) and 6 (final)
#                 suffix = "0/spieltag.html"
#                 )
# 
# page <- get_content(url_vector)
# page
# 
# saveRDS(page, file = "pagecontent.rds")
# 
# # extract result table using xpath and transform into data frame
# result_table <- html_node(page, xpath = "//table[@class='tStat tabdfb-pokal']")
# result_df <- html_table(result_table, fill = TRUE, header = TRUE) #use fill = TRUE due to inconsistent column numbers (at least in round 1)
# 
# result_df <- result_df[, c(3, 5, 6)]
# 
# dim(result_df)
# colnames(result_df)
# 
# # change column names
# colnames(result_df) <- c("home", "away", "result")
# 
# str(result_df)
# 
# # delete empty rows
# result_df <- result_df %>%
#   filter(result != "")
# 
# # check number of rows (=matches)
# dim(result_df)
# 
# # format results
# result_df <- result_df %>%
#   mutate(
#          season = url_vector["season"], #change in function
#          round = url_vector["round"], #change in function
#          result_cleaned = str_trim(str_replace_all(result, "(n\\.V\\.|i\\.E\\.|\\(\\d+:\\d+\\))", "")),
#          extratime = ifelse(str_detect(result, "n.V."), TRUE, FALSE),
#          penalties = ifelse(str_detect(result, "i.E."), TRUE, FALSE),
#          home_goals = str_match(result_cleaned, "(\\d+):")[, 2],
#          away_goals = str_match(result_cleaned, ":(\\d+)")[, 2],
#          winner = as.factor(ifelse(home_goals > away_goals, "home", "away")) # there are no draws
#   ) %>%
#   select(season, round, everything()) #reorder: put season and round to the left
#   
# str(result_df)





# need a list of teams in 1. and 2. Bundesliga for each season. Potential source: table of the respective season
  # alternative: get 3. Bundesliga as well. Then all remaining teams would be 4th division and below.



scrape_round <- function(season, round = 1) {
  if (!round %in% 1:6)  return(NULL)
  url_vector <- c(base = "http://www.kicker.de/news/fussball/dfbpokal/spielrunde/dfb-pokal", 
                  season = season,
                  round = round, # value between 1 (1st round) and 6 (final)
                  suffix = "0/spieltag.html"
  )
  
  page <- get_content(url_vector)
  page
  
  saveRDS(page, file = "pagecontent.rds")
  
  # extract result table using xpath and transform into data frame
  result_table <- html_node(page, xpath = "//table[@class='tStat tabdfb-pokal']")
  result_df <- html_table(result_table, fill = TRUE, header = TRUE) #use fill = TRUE due to inconsistent column numbers (at least in round 1)
  
  result_df <- result_df[, c(3, 5, 6)]
  
  dim(result_df)
  colnames(result_df)
  
  # change column names
  colnames(result_df) <- c("home", "away", "result")
  
  str(result_df)
  
  # delete empty rows
  result_df <- result_df %>%
    filter(result != "")
  
  # check number of rows (=matches)
  dim(result_df)
  
  # format results
  result_df <- result_df %>%
    mutate(
      season = season,
      round = round,
      result_cleaned = str_trim(str_replace_all(result, "(n\\.V\\.|i\\.E\\.|\\(\\d+:\\d+\\))", "")),
      extratime = ifelse(str_detect(result, "n.V."), TRUE, FALSE),
      penalties = ifelse(str_detect(result, "i.E."), TRUE, FALSE),
      home_goals = str_match(result_cleaned, "(\\d+):")[, 2],
      away_goals = str_match(result_cleaned, ":(\\d+)")[, 2],
      winner = as.factor(ifelse(home_goals > away_goals, "home", "away")) # there are no draws
    ) %>%
    select(season, round, everything()) #reorder: put season and round to the left
  
  str(result_df)
  
  result_df  
}

round6 <- scrape_round("2017-18", 6)


