
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

# scrapes all results from one competition stage; returns a formatted tibble with all results from the respective stage
scrape_stage <- function(season, stage = 1) {
  if (!stage %in% 1:7)  return(NULL)
  
  url_vector <- c(base = "http://www.kicker.de/news/fussball/dfbpokal/spielrunde/dfb-pokal", 
                  season = season,
                  stage = stage, # value between 1 (1st stage) and 6 (final)
                  suffix = "0/spieltag.html"
                  )
  
  page <- get_content(url_vector)
  
  # extract result table using xpath and transform into data frame
  result_table <- html_node(page, xpath = "//table[@class='tStat tabdfb-pokal']")
  result_df <- html_table(result_table, fill = TRUE, header = TRUE) #use fill = TRUE due to inconsistent column numbers (at least in stage 1)
  result_df <- result_df[, c(3, 5, 6)]
  
  # change column names
  colnames(result_df) <- c("home", "away", "result")
  
  # delete empty rows and format results
  result_df <- result_df %>%
    filter(result != "") %>%
    mutate(
      season = season,
      stage = stage,
      result_cleaned = str_trim(str_replace_all(result, "(n\\.V\\.|i\\.E\\.|\\(\\d+:\\d+\\))", "")),
      extratime = ifelse(str_detect(result, "n.V."), TRUE, FALSE),
      penalties = ifelse(str_detect(result, "i.E."), TRUE, FALSE),
      home_goals = str_match(result_cleaned, "(\\d+):")[, 2],
      away_goals = str_match(result_cleaned, ":(\\d+)")[, 2],
      winner = as.factor(ifelse(home_goals > away_goals, "home", "away")) # there are no draws
    ) %>%
    select(season, stage, everything()) #reorder: put season and stage to the left
  
  as_tibble(result_df)
}

# scrape full competition results from kicker.de; returns a formatted tibble with all results from that season
scrape_season <- function(season) {
  season_n <- length(season)
  seasons <- vector("list", season_n)
  stages <- vector("list", 7)
  results <- map(seasons, function (x) x[[1]] <- stages)
  
  for (s in 1:season_n) {
    message(str_c("Iterating season ", season[s], "."))
    for (stage in 1:7) {
      message(str_c(" |__ Iterating stage ", stage, "."))
      tryCatch(
        results[[s]][[stage]] <- scrape_stage(season[s], stage),
        error = function(e) return(data.frame())
      )
    }
  }
  results %>% purrr::flatten() %>% data.table::rbindlist() %>% as_tibble()
}

# create correct season format just by starting year
format_year <- function(year) {
  if (!is.numeric(year)) return(NULL)
  fmt <- ""
  if (year == 1999) {
    fmt <- "1999-00"
  }
  else if (year >= 2000 & year < 2009) {
    fmt <- str_c(year, "-", "0", year %% 100 + 1)
  }
  else {
    fmt <- str_c(year, "-", year %% 100 + 1)
  }
}

# get all results from 1963 onwards
pokal <- scrape_season(map_chr(1963:2017, format_year))

saveRDS(pokal, "dfbpokal_allseasons.RData")

count_stages <- pokal %>% count(season, stage)



# need a list of teams in 1. and 2. Bundesliga for each season. Potential source: table of the respective season
# alternative: get 3. Bundesliga as well. Then all remaining teams would be 4th division and below.

