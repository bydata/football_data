library(tidyverse)
library(ggtext)
library(lubridate)
library(glue)

# Source: bwin.de and tipico.de (2020-08-22)
odds <- tribble(
  ~team, ~bwin, ~tipico,
  "Bayern München", 1.15, 1.2,
  "Borussia Dortmund", 6.5, 6.5, 
  "RB Leipzig", 15.0, 20.0,
  "Borussia Mönchengladbach", 51.0, 50.0,
  "Bayer 04 Leverkusen", 67.0, 50.0,
  "FC Schalke 04", 201.0, 600.0,
  "Eintracht Frankfurt", 201.0, 300.0,
  "VfL Wolfsburg", 201.0, 150.0,
  "Hertha BSC", 201.0, 200.0,
  "TSG Hoffenheim", 501.0, 200.0,
  "Werder Bremen", 1001.0, 750.0,
  "1. FC Köln", 1001.0, 600.0,
  "1. FSV Mainz 05", 1001.0, 1250.0,
  "SC Freiburg", 1001.0, 1000.0,
  "FC Augsburg", 1001.0, 1250.0,
  "VfB Stuttgart", 1001.0, 750.0,
  "1. FC Union Berlin", 2001.0, 1500.0,
  "Arminia Bielefeld", 2001.0, 2000.0
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
  mutate(odds_gmean = sqrt(bwin * tipico)) %>% 
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
  mutate(month = month(min(date), label = TRUE, abbr = TRUE, locale = "English_US")) %>% 
  ungroup() %>% 
  mutate(month2 = fct_reorder(month, date, min)) %>% 
  select(-ends_with(".against"), team.against, rank.against, odds_gmean.against, odds_rank_grp.against)


## get club icons (downloaded using xx_download_club_icons.R)
icons_dir <- "input/team_icons/"
icon_files <- list.files(icons_dir, pattern = "*.png")
names(icon_files) <- odds$team
# reorder by odds rank
icon_files <- icon_files[order(odds$rank)]

icon_labels <- sprintf("<img src='%s%s' width='18'>&nbsp;%s", 
                       icons_dir, rev(icon_files),
                       str_pad(18:1, width = 2, side = "left", pad = " "))

# load fonts (download from Google Fonts)
windowsFonts(`DM Serif Display` = windowsFont("DM Serif Display")) 
windowsFonts(Georgia = windowsFont("Georgia"))
windowsFonts(`Open Sans` = windowsFont("Open Sans"))

# colors
tier_shape_colors <- c("#DA4D63", "grey70", "#1D7FB8")
tier_font_colors <- matrix(c("white", "grey10", "white", tier_shape_colors), ncol = 3, byrow = TRUE)

# y position for lines separating teams
lines_y <- 1:17 + 0.5

# x position for lines separating teams
lines_x <- seq(1, 34, 4) + 0.5

# font size for text geoms
geom_text_font_size <- 3

# subtitle
plot_subtitle <- sprintf("Colors indicate opponent's implied strength: <b style='color:%s'>top</b>,
                          <b style='color:%s'>middle</b>, <b style='color:%s'>bottom</b> tier. 
                         Filled shapes denote home matches.", 
                         tier_shape_colors[1], tier_shape_colors[2], tier_shape_colors[3])

# caption
plot_caption <- "Opponent's implied strength: Teams have been ranked based on the pre-season odds of winning the Bundesliga title (as of 22 August 2020).
Odds were obtained from bwin.de and tipico.de. For ranking, the geometric mean of the odds for each team was calculated. 
Ties were solved by picking the first item.\n
Source: github.com/bydata"

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
            size = geom_text_font_size, hjust = "center", vjust = "center", col = tier_font_colors[1, 1]) +
  geom_text(data = . %>% filter(odds_rank_grp.against == "Top" & home_away == "away"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", col = tier_font_colors[2, 1]) +
  # 2nd tier opponents
  geom_text(data = . %>% filter(odds_rank_grp.against == "Middle" & home_away == "home"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", col = tier_font_colors[1, 2]) +
  geom_text(data = . %>% filter(odds_rank_grp.against == "Middle" & home_away == "away"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", col = tier_font_colors[2, 2]) +
  # 3rd tier opponents
  geom_text(data = . %>% filter(odds_rank_grp.against == "Bottom" & home_away == "home"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", col = tier_font_colors[1, 3]) +
  geom_text(data = . %>% filter(odds_rank_grp.against == "Bottom" & home_away == "away"),
            aes(label = rank.against), 
            size = geom_text_font_size, hjust = "center", vjust = "center", col = tier_font_colors[2, 3])

p <- p +
  # draw lines to separate teams
  annotate("segment", x = 0, xend = Inf, y = lines_y, yend = lines_y, col = "grey30") 

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
  labs(title = "Bundesliga fixture difficulty 2020/'21",
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = NULL, shape = "Ground", col = "Opponent's strength")

# custom theme adjustments
p <- p + theme_minimal(base_family = "Open Sans") +
  theme(
    plot.title = element_text(
      family = "DM Serif Display",
      face = "bold",
      size = 24,
      margin = margin(t = 16, b = 10)
    ),
    plot.subtitle = element_markdown(size = 12,
                                     margin = margin(b = 16),
                                     lineheight = 1.2),
    plot.caption = element_text(hjust = 0, margin = margin(t = 10, b = 6), color = "grey40", size = 9),
    text = element_text(color = "grey20"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_markdown(hjust = 0, vjust = 0.5, size = 6),
    strip.placement = "outside",
    strip.text = element_text(vjust = 1, margin = margin(t = 0), size = 10, color = "grey40"),
    panel.spacing = unit(0, "mm"),
    legend.position = "top",
    legend.justification = "left",
    panel.grid = element_blank(),
    plot.margin = margin(l = 12, r = 12, b = 10)
  )

x11(width = 11, height = 8.5)
p

ggsave("plots/bundesliga_fixture_difficulty.png", type = "cairo", dpi = 320, width = 11, height = 8.5)



## Plot per team ----------------------


# labels for facet headers
facet_header_labels <- sprintf("<img src='%s%s' width=20>", 
                       icons_dir, icon_files)  # reminder: already reordered
names(facet_header_labels) <- odds$team[order(odds$rank)]
# turn into tibble
facet_header_labels_tbl <- as_tibble(facet_header_labels, rownames = "team") %>% 
  rename(facet_label = value)

# subtitle
plot_subtitle <- sprintf("Each bar indicates a matchday. Colors indicate opponent's implied strength: <b style='color:%s'>top</b>,
                          <b style='color:%s'>middle</b>, <b style='color:%s'>bottom</b> tier.<br>
                         Only one half of the campaign is displayed.", 
                         tier_shape_colors[1], tier_shape_colors[2], tier_shape_colors[3])

# draw plot
p <- fixtures_odds %>% 
  inner_join(facet_header_labels_tbl, by = "team") %>% 
  # only show the order of teams' opponents and exclude second round
  filter(matchday <= 17) %>% 
  mutate(
    # reorder team by odds rank
    facet_label = fct_reorder(facet_label, rank),
    # center opponents rank around avg rank (9)
    rank_centered.against = 9 - rank.against
    ) %>%
  ggplot(aes(matchday, rank_centered.against))

p <- p +
  geom_col(aes(fill = odds_rank_grp.against),
           width = 0.6, show.legend = FALSE) 

# p <- p +
#   geom_point(data = . %>% filter(home_away == "home"), 
#              aes(y = ifelse(
#                rank_centered.against >= 0, 
#                rank_centered.against + 0.5,
#                rank_centered.against - 0.5
#                ), col = odds_rank_grp.against),
#              size = 0.75,
#              shape = 15,
#              show.legend = FALSE)

p <- p +
  scale_fill_manual(values = tier_shape_colors) +
  scale_color_manual(values = tier_shape_colors)

p <- p +
  facet_wrap(vars(facet_label), ncol = 6)

p <- p + 
  labs(
  title = "Bundesliga fixture difficulty 2020/'21 by team",
  subtitle = plot_subtitle,
  caption = plot_caption
)
  
p <- p +
  theme_minimal() +
  theme(plot.title = element_text(
    family = "DM Serif Display",
    face = "bold",
    size = 24,
    margin = margin(t = 16, b = 10)
  ),
  plot.subtitle = element_markdown(size = 12,
                                   margin = margin(b = 16),
                                   lineheight = 1.2),
  plot.caption = element_text(hjust = 0, margin = margin(t = 10, b = 6), color = "grey40", size = 9),
  text = element_text(color = "grey20"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_markdown(margin = margin(t = 16, b = 8)),
        panel.spacing.x = unit(8, "mm"),
        plot.margin = margin(l = 12, r = 12)
  )

x11(width = 10, height = 6.7)
p

ggsave("plots/bundesliga_fixture_difficulty_by_team.png", type = "cairo", dpi = 320, width = 10, height = 6.7)

