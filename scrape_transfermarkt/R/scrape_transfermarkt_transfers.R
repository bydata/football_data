library(tidyverse)
library(rvest)
library(tictoc)

# get mapping of team names to team ids
scrape_team_mapping <- function(year) {
  message(sprintf("Processing year %d", year))
  # https://www.transfermarkt.de/1-bundesliga/transfers/wettbewerb/L1/plus/?saison_id=2020&s_w=&leihe=1&intern=1
  url <- sprintf("https://www.transfermarkt.de/1-bundesliga/transfers/wettbewerb/L1/plus/?saison_id=%s&s_w=&leihe=1&intern=1", year)
  page <- read_html(url)
  nodes <- html_nodes(page, xpath = "//h2/a[@class='vereinprofil_tooltip']")
  df <- tibble(
    name = html_text(nodes),
    id = html_attr(nodes, name = "id")
  )
  df
}


get_html_table <- function(x) {
  html_table(x, fill = TRUE, header = FALSE) %>% 
    # remove first row
    slice(-1) %>% 
    # select relevant columns
    select(player.name = 4, player.position = 5, player.age = 6, club.name = 10, club.league = 11, fee = 12) %>% 
    # remove empty rows
    na.omit() 
}


prepare_transfer_table <- function(df, type = c("adds", "outs")) {
  df <- df %>% 
    # cleanup fee column
    mutate(
      transfer_type = case_when(
        str_detect(fee, "^Leihe|Leihgebühr:") ~ "Loan",
        str_detect(fee, "^Leih-Ende") ~ "Loan end",
        club.name == "Vereinslos" ~ "Free Agent",
        club.name == "Karriereende" ~ "Career end",
        club.name == "Unbekannt" ~ "Unknown",
        fee == "?" ~ "Transfer (unknown fee)",
        str_count(fee) == 1 ~ "Other",
        fee == "\U2012" ~ "Other",
        TRUE ~ "Transfer"
      ) %>% as.factor(),
      fee_unit = case_when(
        str_detect(fee, "\\Tsd\\.") ~ 10^3,
        str_detect(fee, "\\Mio\\.") ~ 10^6
      ),
      fee_cleaned = str_replace(fee, "(Leihe|Leih-Ende|Leihgebühr:)", "") %>% 
        str_replace("\\s(Tsd|Mio)\\..+$", "") %>% 
        str_replace(",", ".") %>% 
        str_replace("^\U2012$", "0") %>% 
        as.numeric(),
      fee_cleaned = fee_cleaned * fee_unit,
      fee_cleaned = ifelse(fee == "ablösefrei", 0, fee_cleaned),
      club.name = ifelse(club.name %in% c("Vereinslos", "Karriereende"), NA, club.name)
    ) %>% 
    # prevent empty character columns to be considered as logical
    mutate_if(is.logical, as.character)
  
  # rename variable dependent on adds/outs
  if (type == "adds") {
    df <- df %>% 
      rename(selling_club.name = club.name, selling_club.league = club.league)
  } else if (type == "outs") {
    df <- df %>% 
      rename(new_club.name = club.name, new_club.league = club.league)
  }
  df
}

scrape_transfers <- function(team_id, year) {
  url <- sprintf("https://www.transfermarkt.de/bundesliga/transfers/verein/%s/saison_id/%s", team_id, year)
  print(url)
  page <- read_html(url)
  # extract tables using xpath and transform into data frame
  tables <- html_nodes(page, xpath = "//div/div/table")
  # empty list for transfers
  transfers <- list()
  # # additions (table element 2)
  # transfers[["adds"]] <- prepare_transfer_table(tables[[2]], "adds")
  # # additions (table element 3)
  # transfers[["outs"]] <- prepare_transfer_table(tables[[3]], "outs")
  # additions (table element 2)
  transfers[["adds"]] <- get_html_table(tables[[2]])
  # additions (table element 3)
  transfers[["outs"]] <- get_html_table(tables[[3]])
  transfers
}


# scrape final tables all seasons at once and store them in a list (takes a while)
season_years <- 2005:2020
# scrape mapping of team names and ids
tic()
team_mapping <- map_df(season_years, scrape_team_mapping, .id = "year")
team_mapping
toc()


# "translate" year id to years
team_mapping <- 
  team_mapping %>% 
  inner_join(tibble(
    year.id = seq_along(season_years) %>% as.character,
    year = season_years), 
    by = c("year" = "year.id")) %>% 
  select(year = year.y, name, id) %>% 
  arrange(year, name) 


# store results
write_rds(team_mapping, "output/team_mapping.RData")
write_csv(team_mapping, "output/team_mapping.csv")


teams <- unique(team_mapping$id)
scrape_config <- expand.grid(
  team_id = teams,
  year = season_years)
nrow(scrape_config)

# for "clubs" like RB Leipzig we utilize purrr::possibly
scrape_transfers_possibly <- possibly(scrape_transfers, otherwise = NULL)


# tic()
# transfers <- pmap(scrape_config, scrape_transfers_possibly)
# toc()


library(parallel)

# scrape all seasons at once and store them in a list (takes a while)
tic()
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterExport(cl, as.list(unique(c(ls(.GlobalEnv),ls(environment())))),envir = environment())
clusterEvalQ(cl,
             {library(tidyverse)
               library(rvest)
             })
transfers_raw <- clusterMap(cl, scrape_transfers_possibly,
                         scrape_config$team_id, scrape_config$year)

stopCluster(cl)
toc()

# name list items with team id and season
transfers_raw <- transfers_raw %>% 
  set_names(str_c(scrape_config$team_id, scrape_config$year, sep = "/"))

# save results
t <- format(Sys.time(), "%Y%m%d%H%M%S")
write_rds(transfers_raw, sprintf("output/transfers_raw_%s.RData", t))

process_transfers <- function(x, type = c("adds", "outs")) {
  stopifnot(type %in% c("adds", "outs"))
  x %>% map(type) %>%
    # remove empty or null results from list
    discard(is.null) %>% 
    discard(~nrow(.x) == 0) %>% 
    bind_rows(.id = "id") %>%
    as_tibble() %>% 
    prepare_transfer_table(type = type) %>%
    separate(id, into = c("team.id", "year"), sep = "/") %>%
    mutate_at(vars(year), as.numeric) %>%
    inner_join(distinct(team_mapping, name, id), by = c("team.id" = "id")) %>%
    mutate(fee = ifelse(fee == "ablösefrei", 0, fee_cleaned)) %>%
    select(team.name = name, team.id, year, player.name:player.age,
           contains("club."), fee, transfer_type) %>%
    arrange(team.id, year)
}

transfers <- list()
transfers[["adds"]] <- process_transfers(transfers_raw, "adds") 
transfers[["outs"]] <- process_transfers(transfers_raw, "outs") 

glimpse(transfers[["adds"]])

write_rds(transfers, sprintf("output/transfers_%s.RData", t))
