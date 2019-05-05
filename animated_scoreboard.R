library(tidyverse)
#devtools::install_github('thomasp85/gganimate') # install from github due to https://github.com/thomasp85/gganimate/issues/316
library(gganimate)
library(ggthemes)

source("scraping.R")


# get all final standings from from 1963 onwards - use parallelisation
bundesliga_df <- map2_df("1. Bundesliga", 1963:2018, scrape_league_table) 
write_csv(bundesliga_df, "bundesliga_standings.csv")

# calculate cumulative points and fill gaps for years of relegation / non-qualification
bl_cumul <- bundesliga_df %>%
  mutate(club = ifelse(club == "Meidericher SV", "MSV Duisburg", club)) %>%
  group_by(club) %>%
  mutate(points_cumul = cumsum(points_3pt)) %>%
  ungroup() %>%
  complete(club, season) %>%  # add rows for seasons during which clubs didn't play in Bundesliga
  group_by(club) %>%
  fill(points_cumul) %>%
  replace_na(list(points_cumul = 0)) %>%
  ungroup() %>%
  select(club, season, starts_with("points_cumul"))

summary(bl_cumul)


# for the sake of consistency
set.seed(1909)

# add a rank for each club within each season
bl_cumul_ordered <- bl_cumul %>%
  group_by(season) %>%
  mutate(order = rank(-points_cumul, ties = "random")) %>%
  ungroup() %>%
  arrange(season, order)

str(bl_cumul_ordered)


clubs <- bl_cumul_ordered %>% distinct(club) %>% arrange(club) %>% pull(club)
(clubs_n <- length(clubs))
seasons_n <- bl_cumul_ordered %>% distinct(season) %>% count() %>% pull(n)

club_colors = c(
  rep("#999999", clubs_n)
)
names(club_colors) <- clubs

# https://www.schemecolor.com
club_colors["Borussia Dortmund"] <- "#FDE100"
club_colors["1. FC Köln"] <- "#ED1C24"
club_colors["1860 München"] <- "Skyblue"
club_colors["Bayern München"] <- "#DC052D"
club_colors["MSV Duisburg"] <- "#3C4E99"
club_colors["Hamburger SV"] <- "#0A3F86"
club_colors["Werder Bremen"] <- "#1D9053"
club_colors["Eintracht Frankfurt"] <- "#E1000F"
club_colors["FC Schalke 04"] <- "#004D9D"
club_colors["1. FC Nürnberg"] <- "#AD1732"
club_colors["Eintracht Braunschweig"] <- "#FBBF0F"
club_colors["VfB Stuttgart"] <- rgb(226, 31, 22, maxColorValue = 255)
club_colors["1. FC Kaiserslautern"] <- "#C32A28"
club_colors["Bor. Mönchengladbach"] <- rgb(37, 166, 89, maxColorValue = 255)
club_colors["1. FSV Mainz 05"] <- "#ED1C24"
club_colors["Arminia Bielefeld"] <- "#004E95"
club_colors["Tennis Borussia Berlin"] <- rgb(92, 26, 115, maxColorValue = 255)
club_colors["Hertha BSC"] <- "#005CA9"
club_colors["SC Freiburg"] <- rgb(255, 7, 7, maxColorValue = 255)
club_colors["RB Leipzig"] <- "#DE023F"
club_colors["Waldhof Mannheim"] <- rgb(0, 66, 146, maxColorValue = 255)
club_colors["Alemannia Aachen"] <- rgb(247, 227, 22, maxColorValue = 255)
club_colors["VfL Wolfsburg"] <- "#65B32E"
club_colors["VfL Bochum"] <- "#005CA9"
club_colors["Fortuna Düsseldorf"] <- "#DA251D"
club_colors["Karlsruher SC"] <- rgb(0, 55, 152, maxColorValue = 255)
club_colors["FC St. Pauli"] <- rgb(111, 71, 42, maxColorValue = 255)
club_colors["Hannover 96"] <- rgb(23, 157, 51, maxColorValue = 255)
club_colors["Rot-Weiss Essen"] <- rgb(230, 0, 3, maxColorValue = 255)
club_colors["Bayer 04 Leverkusen"] <- "#E32221"
club_colors["TSG Hoffenheim"] <- "#1C63B7"
#club_colors[""] <- ""

club_colors

# how many clubs to display on chart
display_clubs_n <- 25

p <- bl_cumul_ordered %>%
  filter(points_cumul > 0 & order <= display_clubs_n) %>%
  #filter(season <= "1968") %>% 
  ggplot(aes(order, group = club, fill = club)) +
  geom_tile(aes(y = points_cumul/2, height = points_cumul), 
            width = 0.8, color = NA, alpha = 0.6, show.legend = FALSE) +
  geom_text(aes(label = str_c("  ", club)), y = 0, size = 4, vjust = 0.25, hjust = 0) +
  coord_flip() +
  scale_x_reverse(breaks = bl_cumul_ordered$order) +
  labs(
    title = "Ewige Tabelle Bundesliga", 
    subtitle = paste("Top", display_clubs_n, "(Stand {closest_state})"), 
    y = NULL, x = NULL
    ) +
  theme_hc() +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = 0)
  ) +
  scale_fill_manual(values = club_colors)
p

# save gif
fps <- 12
anim <- p + transition_states(season, wrap = FALSE)  + view_follow(fixed_y = TRUE)
animate(anim, nframes = 8 * seasons_n, fps = fps, width = 800, height = 600, end_pause = 4 * fps)
#animate(anim, nframes = 50, fps = 3, width = 1000, height = 800)
anim_save("ewige_tabelle.gif")  

# as a video file
fps <- 25
animate(anim, nframes = ceiling(fps / 2 * seasons_n), fps = fps, width = 800, height = 600, end_pause = 4 * fps, renderer = av_renderer())
anim_save("ewige_tabelle.mp4")

bl_cumul_ordered %>%
  filter(order == 1) %>%
  group_by(club) %>%
  summarize(
    seasons_n = n(),
    season_first = first(season),
    season_last = last(season)
  ) %>%
  arrange(desc(seasons_n))
# only 2 clubs on first rank: Köln & Bay. München (from 1980-81 onwards)

# runner-up
bl_cumul_ordered %>%
  filter(order == 2) %>%
  group_by(club) %>%
  summarize(
    seasons_n = n(),
    season_first = first(season),
    season_last = last(season)
  ) %>%
  arrange(desc(seasons_n))

# top 3
bl_cumul_ordered %>%
  filter(order <= 3) %>%
  group_by(club) %>%
  summarize(
    seasons_n = n(),
    season_first = first(season),
    season_last = last(season)
  ) %>%
  arrange(desc(seasons_n))


bl_cumul_ordered %>%
  filter(points_cumul > 0) %>%
  group_by(club) %>%
  summarize(
    diff = last(order, order_by = season) - min(order),
    top = min(order),
    least = max(order)
    ) %>%
  arrange(desc(diff))

