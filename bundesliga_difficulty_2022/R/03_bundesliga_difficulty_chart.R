library(tidyverse)
library(ggtext)
library(lubridate)
library(glue)
library(systemfonts)
# library(extrafont)

# loadfonts(quiet = TRUE)


# choose language: German or English_US
lang <- "German"


## Data preparation ==================================

# Source: and tipico.de (2021-08-13)
odds <- tribble(
  ~team, ~bwin,
  "Bayern München", 1.2, 
  "Borussia Dortmund", 8,
  "RB Leipzig", 11, 
  "Bayer 04 Leverkusen", 67,
  "Borussia Mönchengladbach", 81,
  "VfL Wolfsburg", 101,
  "Eintracht Frankfurt", 151,
  "Hertha BSC", 301,
  "TSG Hoffenheim", 501, 
  "VfB Stuttgart", 501, 
  "1. FC Union Berlin", 501,
  "1. FSV Mainz 05", 501,
  "SC Freiburg", 501,
  "FC Augsburg", 1001,
  "1. FC Köln", 1001,
  "Arminia Bielefeld", 3001, 
  "VfL Bochum", 3001,
  "SpVgg Greuther Fürth", 5001
)

# read fixtures (scraped with 01_scrape_bundesliga_fixtures.R)
fixtures <- read_rds("output/bundesliga_fixtures.RData")

# check if team names match
fixture_teams <- unique(c(fixtures$home, fixtures$away)) %>% 
  tibble(team = .)
odds %>% 
  anti_join(fixture_teams) # must return an empty dataframe

rm(fixture_teams)

# calculate geometric mean from odds and calculate team rank
odds <- odds %>% 
  # mutate(odds_gmean = sqrt(bwin * tipico)) %>% 
  mutate(odds_gmean = bwin) %>% 
  mutate(rank = rank(odds_gmean, ties.method = "first"))

# create a key for each fixture
fixtures <- fixtures %>% 
  mutate(id = row_number())

# reshape fixtures dataframe so that each team has its row (i.e. two rows for each fixture)
# and add teams title odds
fixtures_odds <- 
  fixtures %>% 
  select(-date_text) %>% 
  pivot_longer(cols = -c("matchday", "year_month", "date", "id"), names_to = "home_away", values_to = "team") %>% 
  inner_join(odds, by = "team") %>% 
  # define groups of odds by number of digits
  mutate(odds_rank_grp = cut(rank, 3, ordered_result = TRUE, labels = FALSE),
         odds_rank_grp = factor(odds_rank_grp, labels = c("1" = "Top", "2" = "Middle", "3" = "Bottom"))) %>%
  # add the other team via self_join 
  # suffix helps to keep the column names from the left side clean
  inner_join(., ., by = "id", suffix = c("", ".against")) %>% 
  # and keep only cases where home_away columns are different
  filter(home_away != home_away.against) %>% 
  # group by matchday to squash matchdays into a single months
  group_by(matchday) %>% 
  mutate(month = month(min(date), label = TRUE, abbr = TRUE, locale = lang)) %>% 
  ungroup() %>% 
  mutate(month2 = fct_reorder(month, date, min)) %>% 
  select(-ends_with(".against"), team.against, rank.against, odds_gmean.against, odds_rank_grp.against)


# get club icons (downloaded using 02_download_club_icons.R)
icons_dir <- "input/team_icons/"
icon_files <- list.files(icons_dir, pattern = "*.png")
names(icon_files) <- odds$team

# reorder by odds rank
icon_files <- icon_files[order(odds$rank)]



# surround icons with HTML <img> tags
icon_labels <- sprintf("<img src='%s%s' width='15'>&nbsp;%s", 
                       icons_dir, rev(icon_files),
                       str_pad(18:1, width = 2, side = "left", pad = " "))


## Plot theme =================================================

base_font_family <- "Source Sans Pro"

lato_bold_italic_path <- system_fonts() %>% 
  filter(family == "Lato", style == "Bold Italic") %>% 
  pull(path)

systemfonts::register_font(
  name = "Lato Bold Italic",
  plain = lato_bold_italic_path
)

chivo_bold_path <- system_fonts() %>% 
  filter(family == "Chivo", style == "Bold") %>% 
  pull(path)

systemfonts::register_font(
  name = "Chivo Bold",
  plain = chivo_bold_path
)

registry_fonts()

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
      plot.background = element_rect(fill = "#fffffb",
                                     color = "#dbdbd3",
                                     size = 0.5), #fafaf2
      plot.title = element_text(
        family = "Chivo Bold",
        color = "black",
        size = 16,
        margin = margin(t = 16, b = 10)
      ),
      plot.subtitle = element_markdown(size = 10,
                                       margin = margin(b = 16),
                                       lineheight = 1.5),
      plot.caption = element_markdown(hjust = 0, 
                                      margin = margin(t = 10, b = 6), 
                                      color = "grey35", size = 9,
                                      lineheight = 1.25),
      text = element_text(color = "grey35"),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      panel.grid = element_blank(),
      plot.margin = margin(l = 12, r = 12, b = 6),
      plot.title.position = "plot"
    )
}
theme_set(theme_custom())




# Titles and labels ===========


titles <- list(
  "English_US" = list(
    title = "Bundesliga fixture difficulty 2021/'22",
    subtitle =  glue(
      "Numbers show projected final position and can be used to identify opponents.<br>
Colors indicate opponent's implied strength: <b style='color:{tier_shape_colors[1]}'>top</b>,
                          <b style='color:grey50'>middle</b>, <b style='color:{tier_shape_colors[3]}'>bottom</b> tier.
                         Filled shapes denote home matches."
    ),
    caption = "Opponent's implied strength: Teams have been ranked based on the pre-season odds of winning the
Bundesliga title (as of 13 August 2021). Odds were obtained from bwin.de.
For ranking, the geometric mean of the odds for each team was calculated.
Ties were solved by picking the first item.\n
Source: github.com/bydata",
    color = "Stärke des Gegners",
    shape = "Heim/Auswärts"
  ),
  "German" = list(
    title  = "Welcher Bundesligaverein spielt wann gegen die schwersten Gegner?",
    subtitle = glue(
      "Die Nummern in den Kästchen geben den Rang der gegnerischen Mannschaft wieder und entsprechen den Zahlen an den Vereinslogos.<br>
Die Farben zeigen die Stärke des Gegners an: <b style='color:{tier_shape_colors[1]}'>Top 6</b>,
<b style='color:grey50'>Mittelfeld</b>, <b style='color:{tier_shape_colors[3]}'>unterste 6</b>.<br>
Die Rangfolge orientiert sich an den Wettquoten (Meister).
Ausgefüllte Kästchen sind Heimspiele."
    ),
    caption = "<b>Quelle:</b> Meisterwette bwin.de (Stand: 13. August 2021) |
    <b>Visualisierung:</b> github.com/bydata<br>
    Die Rangfolge der Vereine wurde auf der Grundlage der Wettquoten für den Gewinn der Meisterschaft ermittelt.\n",
    color = "",
    shape = NULL
  )
)


## Plot ===================================

# y position for lines separating teams
lines_y <- 1:17 + 0.5



ragg::agg_png("plots/bundesliga_fixture_difficulty.png", res = 320,
              width = 6.5, height = 5.5, units = "in", scaling = 1/1.4)

# draw plot
p <- fixtures_odds %>% 
  ggplot(aes(factor(matchday), reorder(team, -rank))) +
  geom_point(aes(shape = home_away, col = odds_rank_grp.against),
             size = 6, alpha = 1)

p <- p +
  # add text separately to not infer with col aesthetic used elsewhere
  # top tier opponents
  geom_text(data = . %>% filter(odds_rank_grp.against == "Top" & home_away == "home"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[1, 1], family = base_font_family) +
  geom_text(data = . %>% filter(odds_rank_grp.against == "Top" & home_away == "away"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[2, 1], family = base_font_family) +
  # 2nd tier opponents
  geom_text(data = . %>% filter(odds_rank_grp.against == "Middle" & home_away == "home"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[1, 2], family = base_font_family) +
  geom_text(data = . %>% filter(odds_rank_grp.against == "Middle" & home_away == "away"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[2, 2], family = base_font_family) +
  # 3rd tier opponents
  geom_text(data = . %>% filter(odds_rank_grp.against == "Bottom" & home_away == "home"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", 
            col = tier_font_colors[1, 3], family = base_font_family) +
  geom_text(data = . %>% filter(odds_rank_grp.against == "Bottom" & home_away == "away"),
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
  guides(shape = FALSE, col = FALSE) +
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

p
invisible(dev.off())
