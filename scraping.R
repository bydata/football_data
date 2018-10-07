
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
      stage = ifelse(season == "1991-92", stage - 4, stage), #correction for pre-cup stages in 1991-92
      result_cleaned = str_trim(str_replace_all(result, "(n\\.V\\.|i\\.E\\.|\\(\\d+:\\d+\\))", "")),
      extratime = ifelse(str_detect(result, "n.V."), TRUE, FALSE),
      penalties = ifelse(str_detect(result, "i.E."), TRUE, FALSE),
      home_goals = as.numeric(str_match(result_cleaned, "(\\d+):")[, 2]),
      away_goals = as.numeric(str_match(result_cleaned, ":(\\d+)")[, 2]),
      winner = factor(ifelse(home_goals > away_goals, "home", "away"), levels = c("home", "away")) # there are no draws
    ) %>%
    filter(home_goals != away_goals) %>% # until the 80s ties were resolved in a rematch; keep only the rematch result
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
    for (stage in 1:7) {
      # data for 1991-92 contain some pre-qualification rounds
      if (season == "1991-92") stage <- stage + 4
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
      # delete empty rows and format results
      result_df %>%
        filter(club != "") %>%
        mutate(
          preseason = str_match(club, "\\s\\((M|P|M,\\sP|P,\\sA|N|A)\\)|\\*")[, 2],
          club = str_trim(str_replace_all(club, "\\s\\((M|P|M,\\sP|(P,\\sA)|(P,\\sN)|N|A)\\)|\\*", "")), #remove indicators for previous year's status
          club = ifelse(club == "1. FC Dynamo Dresden", "Dynamo Dresden", club),
          league = league,
          season = format_year(year),
          goals_scored = as.numeric(str_match(goal_ratio, "(\\d+):")[, 2]),
          goals_against = as.numeric(str_match(goal_ratio, ":(\\d+)")[, 2])
        ) %>%
        select(league, season, everything(), preseason) %>%
        as_tibble()
    },
    error = function(e) return(NULL)
  )
}

# helper function to call scrape_league_table within parLapply
# scrape_league_table_parallel <- function(args) {
#   if (typeof(args) != "list" || is.null(args[["league"]]) || is.null(args[["year"]])) {
#     return(NULL)
#   } 
#   scrape_league_table(args[["league"]], args[["year"]]) 
# }

scrape_season_results <- function(league, year, exclude.na = TRUE) {
  # 2. Bundesliga not supported by now
  if (league == "2. Bundesliga") stop("2. Bundesliga not supported.")
  league_url_parts <- vector("character", 2)
  if (league == "1. Bundesliga") {
    league_url_parts <- c("bundesliga", "1-bundesliga")
  } else if (league == "2. Bundesliga") {
    if (year < 1981 | year == 1991) return(NULL) # introduction of 2. Bundesliga in 1981 and 2 divisions in 1991
    league_url_parts <- c("2bundesliga", "2-bundesliga")
  }
  
  #http://www.kicker.de/ajax.ashx?ajaxtype=kreuztabelle&boxID=tabellen&liganame=1-bundesliga&saison=2017-18&spieltag=0&turniergruppe=0&

  url <- str_c("http://www.kicker.de/ajax.ashx?ajaxtype=kreuztabelle&boxID=tabellen",
               "&liganame=", league_url_parts[2],
               "&saison=", format_year(year),
               "&spieltag=0&turniergruppe=0&")
  
  page <- get_content(url)
  # extract result table using xpath and transform into data frame
  #tryCatch(
   # {
      # returns a data frame without column names and (row) values
      result_table <- html_node(page, xpath = "//table[@class='tStat kreuztab']")
      
      result_df <- html_table(result_table, fill = TRUE, header = FALSE) 
      
      if (year <= 1964) { # 16 teams
        result_df <- result_df %>%
          select(X2:X17) %>%
          na_if("") %>%
          filter(rowSums(is.na(.)) != 16)
      } else if (year == 1991) { # 20 teams
        result_df <- result_df %>%
          select(X2:X21) %>%
          na_if("") %>%
          filter(rowSums(is.na(.)) != 20)
      } else { # 18 teams
        result_df <- result_df %>%
          select(X2:X19) %>%
          na_if("") %>%
          filter(rowSums(is.na(.)) != 18)
      }
      
      # parse column names from td elements
      column_header <- html_nodes(page, xpath = "//table[@class='tStat kreuztab']/tr[1]/td/img") %>%
        html_attr("title")
      
      colnames(result_df) <- column_header
      
      # parse row values
      home_team <- html_nodes(page, xpath = "//table[@class='tStat kreuztab']/tr[*]/td[1]/img") %>%
        html_attr("title")
      
      # parse match day
        match_day_regex <- str_c("/news/fussball/bundesliga/spieltag/", league_url_parts[2], "/\\d{4}-\\d{2}/(\\d{1,2})/")
        match_days_raw <- html_nodes(page, xpath = "//table[@class='tStat kreuztab']/tr[*]/td[*]/a[@class='link_noicon']") %>%
        html_attr("href") %>%
        str_match(match_day_regex)
        
        match_days_vec <- as.numeric(match_days_raw[, 2])
        
        # create a vector with NAs of size = number of teams in league
        no_of_teams <- length(home_team)
        NA_vec <- rep(NA, no_of_teams)
        # create a vector of row-wise positions for self-self matches in cross-table
        cross_vec <- vector("numeric", no_of_teams)
        for (i in 1:no_of_teams) {
          cross_vec[i] <- (i - 1) * no_of_teams + 0.5
        }
        # merge scraped vector of match days with NA vector
        match_days <- c(match_days_vec, NA_vec)
        # create a new vector with indices, every new element get half-ranked in order to be inserted in front of the next element
        ind <- c(seq_along(match_days_vec), cross_vec)
        # re-order based on new index
        match_days <- match_days[order(ind)]
        # create a matrix 
        match_days_mat <- matrix(match_days, ncol = no_of_teams, byrow = TRUE)

      matches <- cbind(home_team, result_df) %>% 
        as_tibble() %>%
        gather(key = away_team, value = result, -home_team, factor_key = FALSE) %>%
        filter(home_team != away_team) %>%
        mutate(
               home_team = as.character(home_team),   
               season = format_year(year),
               home_goals = as.numeric(str_match(result, "(\\d+)-")[, 2]),
               away_goals = as.numeric(str_match(result, "-(\\d+)")[, 2])
               ) %>%
        select(season, home_team, everything())
      
      if (exclude.na) {
        matches <- matches %>%
          filter(!is.na(result))
      }
      # need to invert the match days to have the correct assignment of first and second half of the campaign
      match_days_half <- no_of_teams - 1
      match_days_vec <- ifelse(match_days_vec > match_days_half, match_days_vec - match_days_half, match_days_vec + match_days_half)
      matches <- cbind(matches, match_day = match_days_vec) 
      matches
   # },
   # error = function(e) {
  #    msg = str_c("Error in year ", year)
  #    stop(msg)
  #  }
 # )
}
