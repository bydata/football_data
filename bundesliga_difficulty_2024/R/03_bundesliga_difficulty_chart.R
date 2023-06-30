library(tidyverse)
library(ggtext)
library(lubridate)
library(glue)

# choose language: German or English_US
lang <- "English_US"
locale <- case_when(
  lang == "German" ~ "de_DE.UTF-8",
  TRUE ~ "en_US.UTF-8"
)
plot_filename <- sprintf("bundesliga_fixture_difficulty_%s.png", 
                         switch(lang, "German" = "de", "English_US" = "en"))
plot_filepath <- paste0("bundesliga_difficulty_2024/plots/", plot_filename)

## Data preparation ==================================

# Ranking based on previous season's final table
teams_ranked <- c(
    "Bayern München", 
    "Borussia Dortmund",
    "RB Leipzig",
    "1. FC Union Berlin",
    "SC Freiburg", 
    "Bayer 04 Leverkusen", 
    "Eintracht Frankfurt",
    "VfL Wolfsburg",
    "1. FSV Mainz 05",
    "Borussia Mönchengladbach",
    "1. FC Köln", 
    "TSG Hoffenheim",
    "Werder Bremen",
    "VfL Bochum",
    "FC Augsburg",
    "VfB Stuttgart",
    "1. FC Heidenheim",
    "SV Darmstadt 98")

teams_ranked_df <- data.frame(
  team = teams_ranked,
  rank = 1:18
)

# read fixtures (scraped with 01_scrape_bundesliga_fixtures.R)
fixtures <- read_rds("bundesliga_difficulty_2024/input/bundesliga_fixtures.rds")

# check if team names match
fixture_teams <- unique(c(fixtures$home, fixtures$away)) %>% 
  tibble(team = .)

teams_ranked_df %>% 
  anti_join(fixture_teams) # must return an empty dataframe

rm(fixture_teams)

# create a key for each fixture
fixtures <- fixtures %>% 
  mutate(id = row_number())

# reshape fixtures dataframe so that each team has its row (i.e. two rows for each fixture)
fixtures_teams_ranked <- 
  fixtures %>% 
  # # try to parse a valid date from the date_text column
  # mutate(date = str_extract(date_text, "[0-3][0-9]\\.[01][0-9]\\.202[23]")) %>% 
  select(-date_text) %>% 
  pivot_longer(cols = -c("matchday", "year_month", "date", "id"), names_to = "home_away", values_to = "team") %>% 
  inner_join(teams_ranked_df, by = "team") %>% 
  # define groups of ranks by number of digits
  mutate(rank_grp = cut(rank, 3, ordered_result = TRUE, labels = FALSE),
         rank_grp = factor(rank_grp, labels = c("1" = "Top", "2" = "Middle", "3" = "Bottom"))) %>%
  # add the other team via self_join 
  # suffix helps to keep the column names from the left side clean
  inner_join(., ., by = "id", suffix = c("", ".against")) %>% 
  # and keep only cases where home_away columns are different
  filter(home_away != home_away.against) %>% 
  # group by matchday to squash matchdays into a single months
  group_by(matchday) %>% 
  mutate(month = month(min(date), label = TRUE, abbr = TRUE, locale = locale)) %>% 
  ungroup() %>% 
  mutate(month2 = fct_reorder(month, date, min)) %>% 
  select(-ends_with(".against"), team.against, rank.against, rank_grp.against)


# get club icons (downloaded using 02_download_club_icons.R)
icons_dir <- "bundesliga_difficulty_2024/input/team_icons/"
icon_files <- list.files(icons_dir, pattern = "*.png")
names(icon_files) <- teams_ranked_df$team

# reorder by rank
icon_files <- icon_files[order(teams_ranked_df$rank)]

# surround icons with HTML <img> tags
icon_labels <- sprintf("<img src='%s%s' width='15'>&nbsp;%s", 
                       icons_dir, rev(icon_files),
                       str_pad(18:1, width = 2, side = "left", pad = " "))


## Plot theme =================================================

base_font_family <- "Source Sans Pro"

# colors
tier_shape_colors <- c("#DA4D63", "#DCDCDC", "#1D7FB8")
tier_font_colors <- matrix(c("white", "grey10", "white", tier_shape_colors[1], 
                             "grey40", tier_shape_colors[3]), ncol = 3, byrow = TRUE)

# font size for text geoms
geom_text_font_size <- 3

# custom ggplot2 theme
theme_custom <- function() {
  theme_minimal(base_family = base_font_family) +
    theme( 
      plot.background = element_rect(
        fill = "#fffffb", color = "#dbdbd3", size = 0.5), #fafaf2
      plot.title = element_text(
        family = "Chivo Bold",
        color = "black",
        size = 16,
        margin = margin(t = 16, b = 10)
      ),
      plot.subtitle = element_textbox_simple(
        size = 10, margin = margin(b = 16), lineheight = 1.3,
         width = 0.9, hjust = 0),
      plot.caption = element_markdown(
        hjust = 0, margin = margin(t = 10, b = 6), color = "grey35", size = 9,
        lineheight = 1.25),
      text = element_text(color = "grey35"),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      panel.grid = element_blank(),
      plot.margin = margin(l = 12, r = 12, b = 6),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}
theme_set(theme_custom())


# Titles and labels ===========


titles <- list(
  "English_US" = list(
    title = "Bundesliga fixture difficulty 2023/'24",
    subtitle =  glue(
      "The numbers in the tiles show the opponents' final position from the previous season.
      The same numbers can be found right to each team's logo so that opponents can be identified.
      Colors indicate opponent's implied strength based on this ranking:
      <b style='color:{tier_shape_colors[1]}'>top</b>,
      <b style='color:grey50'>middle</b>, <b style='color:{tier_shape_colors[3]}'>bottom</b> tier.
      Filled shapes denote home matches."
    ),
    caption = "Source: dfb.de, tipico.de. Visualization: Ansgar Wolsing",
    color = "Stärke des Gegners",
    shape = "Heim/Auswärts"
  ),
  "German" = list(
    title  = "Welcher Bundesligaverein spielt wann gegen die schwersten Gegner?",
    subtitle = glue(
      "Die Nummern in den Kästchen geben den Rang der gegnerischen Mannschaft wieder 
      und entsprechen den Zahlen an den Vereinslogos.<br>
Die Farben zeigen die Stärke des Gegners an: <b style='color:{tier_shape_colors[1]}'>Top 6</b>,
<b style='color:grey50'>Mittelfeld</b>, <b style='color:{tier_shape_colors[3]}'>unterste 6</b>.
Die Rangfolge orientiert sich an der Abschlusstabelle der Vorsaison 
(Aufsteiger aus der 2. Bundesliga auf Rang 17 und 18 gesetzt). 
Ausgefüllte Kästchen sind Heimspiele.<br><br>
      <b>Spiele der Saison 2023/'24</b>"
    ),
    caption = "<b>Quelle:</b> Spielplan DFB | <b>Visualisierung:</b> Ansgar Wolsing",
    color = "",
    shape = NULL
  )
)


## Plot ===================================

# y position for lines separating teams
lines_y <- 1:17 + 0.5

# draw base plot
p <- fixtures_teams_ranked %>% 
  ggplot(aes(factor(matchday), reorder(team, -rank))) +
  geom_point(aes(shape = home_away, col = rank_grp.against),
             size = 6, alpha = 1)

p <- p +
  # add text separately to not infer with col aesthetic used elsewhere
  # top tier opponents
  geom_text(data = . %>% filter(rank_grp.against == "Top" & home_away == "home"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[1, 1], family = base_font_family) +
  geom_text(data = . %>% filter(rank_grp.against == "Top" & home_away == "away"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[2, 1], family = base_font_family) +
  # 2nd tier opponents
  geom_text(data = . %>% filter(rank_grp.against == "Middle" & home_away == "home"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[1, 2], family = base_font_family) +
  geom_text(data = . %>% filter(rank_grp.against == "Middle" & home_away == "away"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[2, 2], family = base_font_family) +
  # 3rd tier opponents
  geom_text(data = . %>% filter(rank_grp.against == "Bottom" & home_away == "home"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[1, 3], family = base_font_family) +
  geom_text(data = . %>% filter(rank_grp.against == "Bottom" & home_away == "away"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[2, 3], family = base_font_family)

p <- p +
  # draw lines to separate teams
  annotate("segment", x = 0, xend = Inf, y = lines_y, yend = lines_y, col = "grey30", size = 0.3) 

p <- p + 
  scale_x_discrete(position = "top") +
  scale_y_discrete(name = NULL, labels = icon_labels) +
  scale_shape_manual(values = c("home" = 15, "away" = 22)) +
  scale_color_manual(values = tier_shape_colors)

# facetting for months - use space="free_x" to adapt facet width to different number of matchdays per months
p <- p + 
  facet_grid(cols = vars(month2), scales = "free_x", space = "free_x")

# titles and legend
p <- p +
  guides(shape = "none", col = "none") +
  labs(title = titles[[lang]]$title,
       subtitle = titles[[lang]]$subtitle,
       caption = titles[[lang]]$caption,
       x = NULL, shape = titles[[lang]]$shape, col = titles[[lang]]$color)

# custom theme adjustments
p <- p + theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.y = element_markdown(hjust = 0, vjust = 0.5, size = 6),
  strip.placement = "outside",
  strip.text = element_text(vjust = 1, margin = margin(t = 0), size = 10, color = "grey40"),
  panel.spacing = unit(0, "mm"),
)

# German version
ragg::agg_png(plot_filepath, res = 400,
              width = 6.5, height = 6, units = "in", scaling = 1/1.4)
p
invisible(dev.off())

