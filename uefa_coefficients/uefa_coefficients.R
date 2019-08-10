library(tidyverse)
library(rvest)
library(parallel)
library(ggthemes)
#devtools::install_github("thomasp85/gganimate")
library(gganimate)


# retrieve page content - takes either a URL string or a 
# vector of strings which constitute a url (will be collapsed to one string using "/")
get_content <- function(url) {
  if (is.vector(url)) {
    url <- str_c(url, collapse = "/")
  }
  tryCatch({content <- read_html(url)}, 
           error = function(e) {
             warning(e)
             return(NULL)
           }) 
  content
}


# get uefa coefficient rankings (page: https://de.uefa.com/memberassociations/uefarankings/country/#/yr/2018 // 
# request in background: https://de.competitions.uefa.com/memberassociations/uefarankings/country/libraries//years/2017/)
scrape_uefa_coefficients <- function(year) {
  request_url <- str_c("https://de.competitions.uefa.com/memberassociations/uefarankings/country/libraries//years/", year)
  html_content <- get_content(request_url)
  t <- html_node(html_content, css = "table.table--standings") %>% 
    html_table(fill = TRUE) %>%
    select(country = Land, position = Pos, coefficient = `Pkt.`, no_of_teams = Vereine) %>%
    mutate(country = str_trim(str_sub(country, 5, (str_length(country) - 5) / 2 + 3)), # remove prefix, and keep only the first of the duplicated country names
           position = as.numeric(position),
           coefficient = as.numeric(str_replace(coefficient, ",", ".")),
           no_of_teams = as.numeric(ifelse(str_detect(no_of_teams, "/"), stringi::stri_match_last(no_of_teams, regex = "\\d+"), no_of_teams)) # in rare cases, the number of teams is "1/8" or "1/9". Only take the last number
    ) %>%
    cbind(year)
  t
}


# scraping coefficients from https://kassiesa.home.xs4all.nl/
# Historical coefficients from 1960 onwards
scrape_uefa_coefficients_2 <- function(year) {
  if (year < 1960 | year > 2019) {
    warning(sprintf("Year %.0f is outside the scope of the data source, returning NA.", year))
    return(NA)
  } else if (year >= 1960 & year <= 1998) {
    method <- 1
  } else if (year >= 1999 & year <= 2003) {
    method <- 2
  } else if (year >= 2004 & year <= 2008) {
    method <- 3
  } else if (year >= 2009 & year <= 2017) {
    method <- 4
  } else {
    method <- 5
  }
  request_url <- str_c("https://kassiesa.home.xs4all.nl/bert/uefa/data/", 
               "method", method, "/",
               "crank", year,".html"
               )
  html_content <- get_content(request_url)
  t <- html_node(html_content, css = "table.t1") %>% 
    html_table(fill = TRUE)
  names(t) <- c("rank_no", "X1", "country",
                "prev_year_4", "prev_year_3", "prev_year_2", "prev_year_1", "current_year",
                "coefficient", "no_of_teams")
  t <- t %>%
    select(-X1) %>%
    cbind(year)
  t              
}


#########

coefficients_tbl <- map_dfr(1960:2019, scrape_uefa_coefficients_2) %>% # 2004 data is missing on the UEFA website
  as_tibble()


# fonts
library(extrafont)
font_import()
loadfonts(device = "win")
windowsFonts()

# how many countries to display on chart
display_countries_n <- 10

# create graph
p <- coefficients_tbl %>%
  filter(rank_no <= display_countries_n) %>%
  ggplot(aes(rank_no, group = country, fill = country)) +
  geom_tile(aes(y = coefficient/2, height = coefficient), 
            width = 0.8, color = NA, alpha = 0.7, show.legend = FALSE) +
  geom_text(aes(label = str_c("  ", str_replace(country, " ", "-"))), y = 1, size = 5, vjust = 0.5, hjust = 0, parse = TRUE) +
  geom_text(aes(label = sprintf("%.2f", coefficient), x = rank_no, y = coefficient + 3), vjust = 0) +
  coord_flip(clip = "off") +
  scale_x_reverse(breaks = coefficients_tbl$rank_no) +
  labs(
    title = "UEFA Coefficients Ranking", 
    subtitle = paste("Top", display_countries_n, "Leagues", "({closest_state})"), 
    caption = "Source: kassiesa.home.xs4all.nl",
    y = NULL, x = NULL
  ) +
  scale_fill_viridis_d(option = "D") +
  guides(fill = NULL) +
  theme_hc() + 
  theme(
    text = element_text(family = "Nunito Sans"),
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = 0)
  )

# animation
fps <- 4
years_n <- (2019 - 1960)
anim <- p + transition_states(year, wrap = FALSE) + ease_aes("linear") 
animate(anim, nframes = years_n * fps, fps = fps, width = 800, height = 600, end_pause = 4 * fps, renderer = av_renderer())
anim_save("uefa_coefficients_top10.mp4")


# coefficients over time

# select top 10 countries from 2019
top10_2019 <- coefficients_tbl %>%
  filter(year == 2019, rank_no <= 10) %>%
  pull(country)
top10_2019

top10_ever <- coefficients_tbl %>%
  filter(rank_no <= 10) %>%
  distinct(country) %>%
  pull(country)

top5_ever <- coefficients_tbl %>%
  filter(rank_no <= 5) %>%
  distinct(country) %>%
  pull(country)


p2 <- coefficients_tbl %>%
  filter(country %in% top10_2019) %>%
  #filter(year >= 2017) %>%
  ggplot(aes(year, coefficient, col = country)) +
  geom_line(alpha = 0.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "D") +
  labs(title = "") +
  theme_hc()

anim2 <- p2 + transition_reveal(year)
animate(anim2, width = 800, height = 600, renderer = av_renderer())



p3 <- coefficients_tbl %>%
  filter(country %in% top5_ever) %>%
  #filter(year == 2019) %>%
  ggplot(aes(rank_no, coefficient, col = country)) +
  geom_point(size = 10, alpha = 0.7) +
  geom_point(size = 10, shape = 1) +
  geom_text(aes(label = country, x = rank_no, y = coefficient + 5), angle = 90, hjust = 0) +
  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  scale_x_reverse(limits = c(25, 1), breaks = seq(1, 25, 1)) +
  labs(
    title = "UEFA Ranking and UEFA Coefficients",
    subtitle = "(Year {closest_state})",
    caption = "Based on countries which ever made it to the top 5.\nSource: kassiesa.home.xs4all.nl",
    x = "Country rank", y = "UEFA coefficient"
       ) +
  guides(col = FALSE, fill = FALSE, size = FALSE) +
  theme_hc() + 
  theme(
    text = element_text(family = "Nunito Sans"),
    #axis.text.y = element_blank(), 
    axis.ticks = element_blank(),
    #panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "#DDDDDD"),
    #panel.grid.minor.x = element_line(color = "#DDDDDD"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = 0)
  )

p3

anim3 <- p3 + transition_states(year, wrap = FALSE) + ease_aes("cubic-in-out") 
animate(anim3, nframes = 240, width = 800, height = 600, renderer = av_renderer())
anim_save("uefa_coefficients_ranks.mp4")
animate(anim3, nframes = 240, width = 800, height = 600)
anim_save("uefa_coefficients_ranks.gif")

