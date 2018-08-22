
library(tidyverse)
library(rvest)

url_vector <- c(base = "http://www.kicker.de/news/fussball/dfbpokal/spielrunde/dfb-pokal", 
                season = "2017-18",
                round = 1,
                suffix = "0/spieltag.html"
                )

# retrieve content
get_content <- function(url) {
  if (is.vector(url)) {
    url <- str_c(url, collapse = "/")
  }
  url %>%
      read_html()
}

page <- get_content(url_vector)
page

saveRDS(page, file = "pagecontent.rds")


# extract result table using xpath and transform into data frame
result_table <- html_node(page, xpath = "//table[@class='tStat tabdfb-pokal']")
result_df <- html_table(result_table, fill = TRUE)

dim(result_df)
colnames(result_df)

# change column names
colnames(result_df) <- c("remove1", "remove2", 
                         "home",
                         "remove4",
                         "away", "result",
                         "remove7", "remove8", "remove9", "remove10", "remove11")

# remove irrelevant columns and delete empty rows
result_df <- result_df %>%
  select(-starts_with("remove")) %>%
  filter(home != "")

# add a column for round
result_df$round <- 1

# check number of rows (=matches)
dim(result_df)

# format results
result_df <- result_df %>%
  mutate(result_cleaned = str_trim(str_replace_all(result, "(n\\.V\\.|i\\.E\\.|\\(\\d+:\\d+\\))", "")),
         extratime = ifelse(str_detect(result, "n.V."), TRUE, FALSE),
         penalties = ifelse(str_detect(result, "i.E."), TRUE, FALSE),
         home_goals = str_match(result_cleaned, "(\\d+):")[, 2],
         away_goals = str_match(result_cleaned, ":(\\d+)")[, 2],
         winner = as.factor(ifelse(home_goals > away_goals, "home", "away")) # there are no draws
  )

str(result_df)

