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
  #content <- read_html(url, options = c("RECOVER"))
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


# prepare data frame for animation -> create a rank variable
coefficients_ordered <- coefficients_tbl %>%
  mutate(country = ifelse(country == "Tschechische Rep.", "Tschechien", country)) %>%
  group_by(year) %>%
  mutate(rank_no = rank(-coefficient, ties = "first")) %>%
  ungroup() %>%
  arrange(year, rank_no) %>%
  select(year, rank_no, country, everything())


# how many countries to display on chart
display_countries_n <- 10


# English translations of country names
countries <- coefficients_ordered %>% distinct(country) %>% arrange(country) %>% pull(country)
(countries_n <- length(countries))

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

# add translations to coeefficients dataframe
coefficients_ordered <- 
  coefficients_ordered %>%
  left_join(country_translations, by = c("country" = "country_de"))


# fake it - data for 2004 is missing on UEFA website
coefficients_ordered_2003 <- coefficients_ordered %>%
  filter(year == 2003)

coefficients_ordered_2004 <- coefficients_ordered_2003 %>%
  mutate(year = 2004)

coefficients_ordered_fixed <- coefficients_ordered %>%
  bind_rows(coefficients_ordered_2004)

# fonts
library(extrafont)
font_import()
loadfonts(device = "win")
windowsFonts()

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
fps <- 12
years_n <- (2019 - 1997 - 1)
anim <- p + transition_states(year, wrap = FALSE) + ease_aes("linear") 
animate(anim, nframes = years_n * fps, fps = fps, width = 800, height = 600, end_pause = 4 * fps, renderer = av_renderer())
anim_save("uefa_coefficients.mp4")



