
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
                  stage = stage, # value between 1 (1st stage) and 6 or 7 (final)
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
      home_goals = as.numeric(str_match(result_cleaned, "(\\d+):")[, 2]),
      away_goals = as.numeric(str_match(result_cleaned, ":(\\d+)")[, 2]),
      winner = factor(ifelse(home_goals > away_goals, "home", "away"), levels = c("home", "away")) # there are no draws
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
    #message(str_c("Iterating season ", season[s], "."))
    for (stage in 1:7) {
      # data for 1991-92 contain some pre-qualification rounds
      if (season == "1991-92") stage <- stage + 4
      
      #message(str_c(" |__ Iterating stage ", stage, "."))
      tryCatch(
        results[[s]][[stage]] <- scrape_stage(season[s], stage),
        error = function(e) {
          message(str_c("Error:", season[s], stage, sep = " "))
          return(data.frame())
        }  
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


# scrape league table
scrape_league_table <- function(league, year) {
  
  league_url_parts <- vector("character", 2)
  if (league == "1. Bundesliga") {
    league_url_parts <- c("bundesliga", "1-bundesliga")
  } else if (league == "2. Bundesliga") {
    if (year < 1981 | year == 1991) return(NULL) # introduction of 2. Bundesliga in 1981 and 2 divisions in 1991
    league_url_parts <- c("2bundesliga", "2-bundesliga")
  }
  
  url <- str_c("http://www.kicker.de/news/fussball",
               league_url_parts[1],
               "spieltag",
               league_url_parts[2],
               format_year(year),
               "spieltag.html",
               sep = "/"
  )
  
  page <- get_content(url)
  # extract result table using xpath and transform into data frame
  tryCatch(
    {
      result_table <- html_node(page, xpath = "//table[@class='tStat']")
      result_df <- html_table(result_table, fill = TRUE, header = TRUE) #use fill = TRUE due to inconsistent column numbers (at least in stage 1)
      result_df <- result_df[, c(1, 3, 5, 7, 8, 9, 11, 12, 14)]
      colnames(result_df) <- c("seed", "club", "games", "W", "D", "L", "goal_ratio", "goal_diff", "points")
    },
    error = function(e) return(NULL)
  )
  # delete empty rows and format results
  result_df %>%
    filter(club != "") %>%
    mutate(
      club = str_trim(str_replace(club, "\\s\\((M|P|M,\\sP|N|A)\\)", "")), # remove indicators for previous year's status
      league = league,
      season = format_year(year),
      goals_scored = as.numeric(str_match(goal_ratio, "(\\d+):")[, 2]),
      goals_against = as.numeric(str_match(goal_ratio, ":(\\d+)")[, 2])
    ) %>%
    select(league, season, everything()) %>%
    as_tibble()
}
