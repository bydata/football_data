library(tidyverse)
library(parallel)
library(ineq)

# scrape results - only run once and then comment out to save runtime
source("scraping.R")

scrape_bundesliga <- function(year) {
  scrape_league_table("1. Bundesliga", year)
}

# get all final standings from from 1963 onwards - use parallelisation
system.time( {
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterExport(cl, as.list(unique(c(ls(.GlobalEnv),ls(environment())))),envir=environment())
  clusterEvalQ(cl,
               {library(tidyverse)
                 library(rvest)
               }
  )
  bundesliga_df <- parLapply(cl, 1963:2017, scrape_bundesliga) %>%
    data.table::rbindlist() %>%
    as_tibble() %>%
    mutate(points_3pt = W * 3 + D,
           year = as.numeric(str_split_fixed(season, "-", n = 2)[, 1])
           ) 
  
  stopCluster(cl)
  cl <- NULL
})


# GINI

ginis <- bundesliga_df %>%
  group_by(year) %>%
  summarize(gini_coeff = Gini(points_3pt)) %>%
  mutate(year = year - 1963)

ggplot(ginis, aes(year, gini_coeff)) +
  geom_point() +
  geom_smooth(col = "red") +
  coord_cartesian(ylim = c(0, 0.25)) 
  
gini_model <- lm(gini_coeff ~ year, ginis)
summary(gini_model)


# ratio point difference between champion and runner-up and runner-up and last non-relegation spot
laplace_est <- 0.01 # add a tiny number
point_diffs <- bundesliga_df %>%
  group_by(year) %>%
  summarize(
    diff1to2 = abs(first(points_3pt) - nth(points_3pt, 2)),
    diff2to15 = nth(points_3pt, 2) - nth(points_3pt, 15),
    ratio = (diff1to2 + laplace_est) / (diff2to15 + laplace_est)
  ) %>%
  mutate(decade = floor(year / 10) * 10)

ggplot(point_diffs, aes(year, ratio)) +
  geom_point() + 
  geom_smooth()

ggplot(point_diffs, aes(diff1to2, diff2to15, col = factor(decade), size = ratio)) +
  geom_point() +
  labs(col = "Decade", x = "Champion to runner-up", y = "Runner-up to 16th", size = "Ratio") +
  ggthemes::theme_fivethirtyeight()

ggsave("point_difference_2to15.png", dpi = "print")

# ratio point difference between champion and runner-up and runner-up and 6th place
laplace_est <- 0.01 # add a tiny number
point_diffs_2 <- bundesliga_df %>%
  group_by(year) %>%
  summarize(
    diff1to2 = abs(first(points_3pt) - nth(points_3pt, 2)),
    diff2to6 = nth(points_3pt, 2) - nth(points_3pt, 6),
    ratio = (diff1to2 + laplace_est) / (diff2to6 + laplace_est)
  ) %>%
  mutate(decade = floor(year / 10) * 10)

ggplot(point_diffs_2, aes(year, ratio)) +
  geom_point() + 
  geom_smooth()

ggplot(point_diffs_2, aes(diff1to2, diff2to6, col = factor(decade), size = ratio)) +
  geom_point() +
  labs(col = "Decade", x = "Champion to runner-up", y = "Runner-up to 6th", size = "Ratio") +
  ggthemes::theme_fivethirtyeight()

ggsave("point_difference_2to6.png", dpi = "print")



# BVB seed across all Bundesliga seasons

seed_chart <- function(club_name, colors = c("#008FD5", "#FF2700"), year_start = 1963, year_end = 2017) {
  bundesliga_df %>%
    filter(club == club_name) %>%
    mutate(seed_reversed = 19 - seed,
           champion = seed == 1) %>%
    ggplot(aes(year, seed_reversed, fill = champion)) +
      geom_col() +
      geom_label(aes(year, seed_reversed, label = seed), inherit.aes = FALSE) +
      coord_cartesian(xlim = c(year_start, year_end), ylim = c(0, 18)) +
      ggthemes::theme_fivethirtyeight() +
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            legend.position="none", panel.grid.major=element_blank()) +
      scale_fill_manual(values = colors)
}
seed_chart("Borussia Dortmund", colors = c("#ffcc00", "black"))
seed_chart("Bayern München", colors = c("red", "blue"))
seed_chart("MSV Duisburg")
seed_chart("Hamburger SV", colors = c("blue", "black"))



# number of different champions per decade
bundesliga_df %>%
  mutate(decade = floor(year / 10) * 10) %>%
  filter(seed == 1) %>%
  group_by(decade) %>%
  summarize(different_champions = n_distinct(club),
            success_title_defence_c = sum(str_detect(preseason, "M"), na.rm = TRUE)) %>%
  ggplot(aes(decade, different_champions, fill = success_title_defence_c)) +
  geom_col() +
  ggthemes::theme_fivethirtyeight()


  