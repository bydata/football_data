library(tidyverse)
library(rvest)
library(parallel)
library(ggthemes)


# retrieve page content - takes either a URL string or a vector of strings which constitute a url (will be collapsed to one string using "/")
get_content <- function(url) {
  if (is.vector(url)) {
    url <- str_c(url, collapse = "/")
  }
  #content <- read_html(url, options = c("RECOVER"))
  tryCatch({content <- read_html(url)}, 
           error = function(e) {
             warning(e)
             return(NULL)
             }) 
  content
}


#https://www.fussballdaten.de/vereine/borussia-dortmund/1995/kader/

scrape_fussballdaten_squad <- function(team, league, year = 2019) {
  # team <- "borussia-dortmund"
  # league <- "bundesliga"
  # year <- 2019
  request_url <- str_c("https://www.fussballdaten.de/vereine", team, year, "kader", sep = "/")
  raw_table <- get_content(request_url) %>% html_node(css = "table.verein-kader")
  
  # parse table
  t <- raw_table %>% html_table(fill = TRUE) 
  colnames(t) <- c("jersey", "name", "X1", "position", "age", "height", "weight", "games",
                   "goals", "assists", "own_goals", "booked", "sent_off_yellow", "sent_off_red", 
                   "subbed_in", "subbed_out", "minutes", "rating", "X2", "X3") 
  t <- t %>% select(-X1, -X2, -X3)  # country is empty
  t <- t %>% filter(!str_detect(jersey, "Spieler:"))
  
  # get nationality from flags (looks a bit overcomplicated but we need to catch edge cases with missing <b> elements containing the flag)
  flag_cells <- html_nodes(raw_table, xpath = "//tr[*]/td[3]")
  flags <- map(flag_cells, ~ html_node(.x, css = "span.flag-icon") %>% html_attr("title")) %>% unlist()
  # be careful, the vector contains the nationalities of the coaching staff as well, first line is empty
  players_n <- nrow(t)
  flags <- flags[2:(players_n+1)]
  

  # convert strings to integers
  convert_str2int <- function(s) {
    as.numeric(str_replace(s, "-", "0"))
  }
 
  if (is.null(flags) || is.null(t)) {
    return(NULL)
  }
  
  # merge
  squad <- cbind(league, year, team, t, flags) %>%
    rename(flag = flags) %>%
    mutate(jersey = as.numeric(jersey),
           #age = as.numeric(age),
           games = convert_str2int(games),
           goals = convert_str2int(goals),
           #assists = convert_str2int(assists),
           height = as.numeric(str_replace(height, ",", ".")),
           weight = as.numeric(str_replace(weight, ",", ".")),
           rating = convert_str2int(rating),
           rating = ifelse(rating == 0, NA, rating)
           )
  squad
} 
  
# returns a character vector of teams for given league and year
scrape_fussballdaten_teams <- function(league, year = 2019) {
  url <- str_c("https://www.fussballdaten.de", league, year, "tabelle", sep = "/")
  team_urls <- get_content(url) %>%
    html_node(css = "div#myTab_tabellen-tab0") %>%
    html_nodes(css = "a.table-link") %>%
    html_attr("href")
  teams <- str_match(team_urls, "/vereine/(.+?)/(\\d{4})/")[, 2:3]
  teams
}  
  

scrape_fussballdaten_squads_parallel <- function(leagues) {
  team_names <- vector("list", length(leagues))
  team_squads <- vector("list", length(leagues))
  for (i in 1:length(leagues)) {
    message(leagues[i])
    tryCatch(
      { team_names[[i]] <- scrape_fussballdaten_teams(leagues[i]) },
      error = function(e) {
        warning(e)
        return(team_squads)
      }
    )
    no_of_teams <- nrow(team_names[[i]])
    team_squads[[i]] <- vector("list", no_of_teams)
    for (j in 1:no_of_teams) {
      message(str_c("|__", j, team_names[[i]][[j]], sep = " "))
      tryCatch(
        {team_squads[[i]][[j]] <- scrape_fussballdaten_squad(team = team_names[[i]][[j]], league = leagues[i])},
        error = function(e) {
          warning(e)
          return(team_squads)
          }
      )
    }
  }
  team_squads
}



teams1 <- scrape_fussballdaten_teams("bundesliga") 
teams1
league1 <- scrape_fussballdaten_squad("borussia-dortmund", "bundesliga")


# run queries for selected leagues
system.time( {
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, as.list(unique(c(ls(.GlobalEnv),ls(environment())))),envir=environment())
  clusterEvalQ(cl,
               {library(tidyverse)
                 library(rvest)}
                )
  leagues <- c("bundesliga", "irland", "frankreich", "italien", "england", "belgien", "bulgarien",
               "daenemark", "finnland", "griechenland", "israel", "kroatien", "niederlande",
               "norwegen", "oesterreich", "polen", "portugal", "rumaenien", "russland", "schottland",
               "schweden", "schweiz", "serbien", "spanien", "tschechien", "tuerkei", "ukraine", "ungarn")
  result <- parLapply(cl, leagues, scrape_fussballdaten_squads_parallel)
  stopCluster(cl)
  cl <- NULL
})

# format player data
players <- data.table::rbindlist(flatten(flatten(result))) %>%
  mutate(league = as.character(league),
         team = as.character(team),
         flag = as.character(flag),
         country = ifelse(league == "bundesliga", "Deutschland", str_to_title(league, locale = "de")),
         country = ifelse(country == "Daenemark", "Dänemark", country),
         country = ifelse(country == "Tuerkei", "Türkei", country),
         country = ifelse(country == "Oesterreich", "Österreich", country),
         country = ifelse(country == "Irland", "Republik Irland", country),
         country = ifelse(country == "Rumaenien", "Rumänien", country), 
         country = ifelse(country == "Tschechien", "Tschechische Rep.", country)
         )

saveRDS(players, "players.RData")


# get uefa coefficient rankings (page: https://de.uefa.com/memberassociations/uefarankings/country/#/yr/2018 // 
# request in background: https://de.competitions.uefa.com/memberassociations/uefarankings/country/libraries//years/2017/)

scrape_uefa_coefficients <- function(year) {
  request_url <- str_c("https://de.competitions.uefa.com/memberassociations/uefarankings/country/libraries//years/", year)
  html_content <- get_content(request_url)
  t <- html_node(html_content, css = "table.table--standings") %>% 
    html_table( fill = TRUE) %>%
    select(country = Land, position = Pos, coefficient = `Pkt.`, no_of_teams = Vereine) %>%
    mutate(country = str_trim(str_sub(country, 5, (str_length(country) - 5) / 2 + 3)), # remove prefix, and keep only the first of the duplicated country names
           position = as.numeric(position),
           coefficient = as.numeric(str_replace(coefficient, ",", ".")),
           no_of_teams = as.numeric(ifelse(str_detect(no_of_teams, "/"), stringi::stri_match_last(no_of_teams, regex = "\\d+"), no_of_teams)) # in rare cases, the number of teams is "1/8" or "1/9". Only take the last number
           ) %>%
    cbind(year)
  t
}

coefficients_tbl <- map_dfr(c(1997:2003, 2005:2019), scrape_uefa_coefficients) %>% # 2004 data is missing on the UEFA website
  as_tibble()


## EXPLORATION: players

countries <- players %>% count(country)
countries

players %>% 
  group_by(league) %>%
  summarize(jersey_mean = mean(jersey, na.rm = TRUE),
            jersey_sd = sd(jersey, na.rm = TRUE)
  ) %>%
  arrange(desc(jersey_mean))

# missing shirt numbers
players %>%
  filter(is.na(jersey)) %>%
  count(country) %>%
  arrange(desc(n))

# missing shirt numbers but matches played
players %>%
  filter(is.na(jersey), games > 0) %>%
  count(country) %>%
  arrange(desc(n))

# teams seem to have A LOOOOT of players in their roster
players %>%
  count(country, team) %>%
  group_by(country) %>%
  summarize(med_players_team = median(n)) %>%
  arrange(desc(med_players_team))

# keep only players with a non-missing shirt number
players_cleaned <- players %>%
  filter(!is.na(jersey))


# number of matches per player and country
players %>%
  group_by(country) %>%
  summarize(avg_no_matches = mean(matches)) %>%
  arrange(avg_no_matches)


## EXPLORATION: UEFA coefficients

# there is an odd dip in (mean) coefficients between 2005 and 2007 - standardize coefficients in order to avoid artefacts
coefficients_tbl %>%
  group_by(year) %>%
  summarize(coeff_mean = mean(coefficient),
            coeff_sd = sd(coefficient)) %>%
  ggplot(aes(year, coeff_mean)) +
  geom_line()

# z-standardize coefficients by year
coefficients_tbl <- coefficients_tbl %>%
  group_by(year) %>%
  mutate(coefficient_z = scale(coefficient)) %>%
  ungroup()

# check if z-standardization worked (mean = 0, sd = 1)
coefficients_tbl %>%
  group_by(year) %>%
  summarize(coeff_z_mean = round(mean(coefficient_z)),
            coeff_z_sd = round(sd(coefficient_z)))
  
coefficients_tbl %>%  
  filter(country %in% c("Spanien", "Deutschland", "England", "Italien", "Frankreich")) %>%
ggplot(aes(year, coefficient_z, col = country)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(coefficients_tbl$coefficient_z) + 1)) + 
  theme_hc() + scale_color_hc()


#devtools::install_github("thomasp85/gganimate")
library(gganimate)

coefficients_ordered <- coefficients_tbl %>%
  mutate(country = ifelse(country == "Tschechische Rep.", "Tschechien", country)) %>%
  group_by(year) %>%
  mutate(rank_no = rank(-coefficient, ties = "first")) %>%
  ungroup() %>%
  arrange(year, rank_no) %>%
  select(year, rank_no, country, everything())

# how many countries to display on chart
display_countries_n <- 10

# country colours
countries <- coefficients_ordered %>% distinct(country) %>% arrange(country) %>% pull(country)
(countries_n <- length(countries))

# English translations


countries_en <- c(
  "Albania", "Andorra", "Armenia", "Azerbaidzhan", "Belarus",
  "Belgium", "Bosnia-Herzegovina", "Bulgaria", "Denmark", "Germany",
  "England", "Estonia", "Färöer", "Finland", "France",
  "Georgia", "Gibraltar", "Greece", "Iceland", "Israel",
  "Italy", "Kazakstan", "Kosovo", "Croatia", "Latvia", 
  "Liechtenstein", "Lithania", "Luxembourg", "Malta", "Moldavia", 
  "Montenegro", "Netherlands", "Northern_Ireland", "North_Mazedonia", "Norway",
  "Austria", "Poland", "Portugal", "Ireland", "Romania", 
  "Russia", "San Marino", "Scotland", "Sweden", "Switzerland",
  "Serbia", "Slovakia", "Slovenia", "Spain", "Czech_Republic",
  "Turkey", "Ukraine", "Hungary", "Wales", "Cyprus"
)

country_translations <- bind_cols(country_de = countries, country_en = countries_en)

coefficients_ordered <- 
  coefficients_ordered %>%
  left_join(country_translations, by = c("country" = "country_de"))

country_colors = c(
  rep("#999999", countries_n)
)
names(country_colors) <- countries
country_colors

country_colors["Deutschland"] <- "black"
country_colors["Spanien"] <- "yellow"
country_colors["England"] <- "red"
country_colors["Frankreich"] <- "blue"
country_colors["Italien"] <- "green"
country_colors["Belgien"] <- "#555555"
#country_colors[""] <- ""


# flags
#img_germany <- readPNG(system.file("flags", "germany.png", package="png"))
# img_germany <- readPNG("flags/germany.png")
# flag_germany <- rasterGrob(img_germany, interpolate=TRUE)


coefficients_ordered_2003 <- coefficients_ordered %>%
  filter(year == 2003)

# fake it
coefficients_ordered_2004 <- coefficients_ordered_2003 %>%
  mutate(year = 2004)

coefficients_ordered_fixed <- coefficients_ordered %>%
  bind_rows(coefficients_ordered_2004)
  

# create graph
p <- coefficients_ordered_fixed %>%
  filter(rank_no <= display_countries_n) %>%
  ggplot(aes(rank_no, group = country_en, fill = country_en)) +
  geom_tile(aes(y = coefficient/2, height = coefficient), 
            width = 0.8, color = NA, alpha = 0.7, show.legend = FALSE) +
  geom_text(aes(label = str_c("  ", country_en)), y = 1, size = 5, vjust = 0.5, hjust = 0, parse = TRUE) +
  geom_text(aes(label = sprintf("%.2f", coefficient), x = rank_no, y = coefficient + 3), vjust = 0) +
  coord_flip(clip = "off") +
  scale_x_reverse(breaks = coefficients_ordered_fixed$rank_no) +
  labs(
    title = "UEFA Coefficients Ranking", 
    subtitle = paste("Top", display_countries_n, "Leagues", "({closest_state})"), 
    caption = "Source: UEFA.com",
    y = NULL, x = NULL
  ) +
  scale_fill_viridis_d(option = "D") +
  guides(fill = NULL) +
  theme_hc() + 
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    #plot.margin = unit(c(0.5, 0.5, 0.5, 2), "cm"),
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = 0)
  )

# animation
fps <- 12
years_n <- (2019 - 1997 - 1)
anim <- p + transition_states(year, wrap = FALSE) + ease_aes("linear") 
animate(anim, nframes = years_n * fps, fps = fps, width = 800, height = 600, end_pause = 4 * fps, renderer = av_renderer())
anim_save("uefa_coefficients.mp4")






# biggest improvements in (scaled) ratings
coefficients_tbl %>%
  filter(year == min(year) | year == max(year)) %>%
  mutate(year = ifelse(year == min(year), "min", "max")) %>%
  arrange(country, year) %>%
  spread(year, coefficient_z, sep = "_") %>%
  group_by(country) %>%
  mutate(year_min = min(year_min, na.rm = TRUE),
         year_max = max(year_max, na.rm = TRUE),
         diff = year_max - year_min
         ) %>%
  ungroup() %>%
  distinct(country, diff) %>%
  arrange(desc(diff))

#####

# join shirt number stats per country with UEFA coefficient
joined <- players %>%
  group_by(country, year) %>%
  summarize(shirt_mean = mean(shirt_number, na.rm = TRUE),
            shirt_sd = sd(shirt_number, na.rm = TRUE)
  ) %>%
  arrange(desc(shirt_mean)) %>%
  inner_join(coefficients_tbl, by = c("country", "year"))

# check which countries remained from LHS
joined %>%
  distinct(country)

# check which countries are missing
players %>%
  anti_join(coefficients_tbl, by = c("country", "year")) %>%
  count(country)

ggplot(joined, aes(coefficient_z, shirt_mean, col = country)) +
  geom_point(aes(size = shirt_sd)) +
  coord_cartesian(xlim = c(-1, 4), ylim = c(6, 40)) +
  labs(size = "Standard deviation of shirt numbers", col = "Country") +
  ggtitle("Mean shirt number by standardized UEFA coefficient") +
  ggthemes::theme_fivethirtyeight()

ggplot(joined, aes(coefficient, shirt_mean, col = country)) +
  geom_point(aes(size = shirt_sd)) +
  coord_cartesian(ylim = c(6, 40)) +
  labs(size = "Standard deviation of shirt numbers", col = "Country") +
  ggtitle("Mean shirt number by UEFA coefficient") +
  ggthemes::theme_fivethirtyeight()

ggsave("shirt_numbers_by_uefacoefficient.png")

ggplot(joined, aes(coefficient, shirt_sd, col = factor(country))) +
  geom_point() +
  ggthemes::theme_fivethirtyeight()



cor(joined$coefficient_z, joined$shirt_mean)
cor(joined$coefficient_z, joined$shirt_sd)

mod1 <- lm(shirt_mean ~ coefficient_z, joined)
summary(mod1)

mod1 <- lm(shirt_mean ~ coefficient_z + no_of_teams, joined)
summary(mod1)



# quantify the number of shirt numbers exceeding the total number of players within each team
players %>%
  group_by(team) %>%
  filter(!is.na(shirt_number)) %>%
  mutate(no_of_players = n(),
         diff = shirt_number - no_of_players) %>%
  filter(matches > 0) %>% # filter separately not to confound the number of players in the squad
  filter(team == "ac-mailand") %>%
  filter(shirt_number > no_of_players)



big_shirts <- players %>%
  group_by(country, year, team) %>%
  filter(!is.na(shirt_number)) %>%
  mutate(no_of_players = n(),
         diff = shirt_number - no_of_players) %>%
  filter(matches > 0 & shirt_number > no_of_players) %>% 
  summarize(players_w_big_shirts = n(),
            median_diff = median(diff)
            )  %>%
  summarize(median_big_shirts = median(players_w_big_shirts),
            median_diff = median(median_diff)
            ) 

big_shirts_coeffs <- big_shirts %>%
  inner_join(coefficients_tbl, by = c("country", "year"))

ggplot(big_shirts_coeffs, aes(coefficient_z, median_diff, col = country)) +
  geom_point(aes(size = median_big_shirts)) +
  ggthemes::theme_fivethirtyeight()

ggplot(big_shirts_coeffs, aes(coefficient_z, median_big_shirts, col = country)) +
  geom_point(aes(size = median_diff)) +
  ggthemes::theme_fivethirtyeight()


ggplot(big_shirts_coeffs, aes(no_of_teams, median_big_shirts, col = country)) +
  geom_point(aes(size = median_diff)) +
  ggthemes::theme_fivethirtyeight()

cor(big_shirts_coeffs$coefficient_z, big_shirts_coeffs$median_diff)
cor(big_shirts_coeffs$coefficient_z, big_shirts_coeffs$median_big_shirts)


# quantify the number of shirt numbers exceeding 50
(shirt50 <- players %>%
  group_by(country, year) %>%
  filter(!is.na(shirt_number)) %>%
  mutate(shirt50 = (shirt_number > 50)) %>%
  summarize(shirt50_share = mean(shirt50)) %>%
  arrange(desc(shirt50_share))
)

players %>%
  filter(country == "Belgien" & team == "aa-gent") %>%
  arrange(desc(shirt_number))

shirt50_coeff <- shirt50 %>%
  inner_join(coefficients_tbl, by = c("country", "year"))

ggplot(shirt50_coeff, aes(coefficient, shirt50_share)) +
  geom_point(size = 2) +
  ggthemes::theme_fivethirtyeight()


cor(shirt50_coeff$shirt50_share, shirt50_coeff$coefficient)

