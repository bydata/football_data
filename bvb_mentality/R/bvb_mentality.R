library(tidyverse)
library(ggtext)

# load Bundesliga tables and fixture results (scraped from dfb.de)
tables <- read_rds("../scrape_bundesliga_tables/output/bundesliga_all_tables_df.RData")
results <- read_rds("../scrape_bundesliga_tables/output/bundesliga_results_crosstable.RData")
results <- bind_rows(results)

glimpse(tables)
glimpse(results)

tables <- tables %>% rename(year = start_year)

# select Klopp and Favre (completed) seasons
# Klopp: 2008-09 to 2014-15
# Favre: 2018-19 to 2019-20
years_klopp <- 2009:2015
years_favre <- 2019:2020

bvb <- "Borussia Dortmund"

tables_opponents <- tables %>% 
  filter(team != bvb, year %in% c(years_klopp, years_favre)) %>% 
  mutate(bvb_manager = case_when(
    year %in% years_klopp ~ "Klopp",
    year %in% years_favre ~ "Favre"
  )) %>% 
  group_by(year) %>% 
  # keep only the final table
  filter(played == max(played)) %>% 
  # group teams in 3 tiers
  mutate(tier = cut(position, 3, labels = FALSE) %>% factor()) %>% 
  ungroup %>% 
  select(season, year, position, tier, team, bvb_manager)


# results by team (i.e. duplicate each fixture)
team_results <- results %>% 
  mutate(key = str_c(home, away, sep = "|")) %>% 
  pivot_longer(cols = c(home, away),
               names_to = "turf",
               values_to = "team") %>% 
  mutate(goals = ifelse(turf == "home", home_goals, away_goals),
         goals_against = ifelse(turf == "home", away_goals, home_goals),
         goal_diff = goals - goals_against,
         outcome = case_when(goal_diff > 0 ~ "won",
                             goal_diff < 0 ~ "lost",
                             TRUE ~ "draw"),
         # parse opponent's name from fixture key
         opponent = str_remove(key, str_c("\\|?", team, "\\|?"))) %>%
  select(season, team, opponent, turf, starts_with("goal"), outcome) 

# all BVB fixtures
bvb_fixtures <- team_results %>% 
  filter(team == bvb) %>% 
  # add tier
  inner_join(tables_opponents, by = c("season", "opponent" = "team")) 

# opponents' goal difference by season
team_goal_diff <- team_results %>% 
  group_by(season, team) %>% 
  summarize(mean = mean(goal_diff),
            sd = sd(goal_diff),
            .groups = "drop"
            )
glimpse(team_goal_diff)


# opponents' goal difference by season in tiers
tier_goal_diff <- team_results %>% 
  inner_join(tables_opponents, by = c("season", "team")) %>% 
  group_by(season, tier) %>% 
  summarize(mean = mean(goal_diff),
            sd = sd(goal_diff),
            median = median(goal_diff),
            perc_05 = unname(quantile(goal_diff, probs = 0.05)),
            perc_16 = unname(quantile(goal_diff, probs = 0.16)),
            perc_25 = unname(quantile(goal_diff, probs = 0.25)),
            perc_75 = unname(quantile(goal_diff, probs = 0.75)),
            perc_84 = unname(quantile(goal_diff, probs = 0.84)),
            perc_95 = unname(quantile(goal_diff, probs = 0.95)),
            .groups = "drop"
  )

str(tier_goal_diff)


## Setup for graphs ==============================================================

# load fonts (download from Google Fonts)
library(extrafont)
font_import(pattern = "Source|OpenSans|Inconsolata", prompt = FALSE)
loadfonts()
base_font_family <- "Open Sans Light"

# library(showtext)
# font_add_google("Open Sans")
# font_add_google("Source Serif Pro")
# font_add_google("Inconsolata")
# showtext_auto()
# base_font_family <- "Open Sans Light"

# font size for text geoms
geom_text_font_size <- 3

# custom ggplot2 theme
theme_custom <- function() {
  theme_minimal(base_family = base_font_family) +
    theme(
      plot.background = element_rect(
        fill = "#fffffb",
        color = "#dbdbd3",
        size = 0.5
      ),
      #fafaf2
      plot.title = element_text(
        family = "Source Serif Pro SemiBold",
        face = "bold",
        size = 20,
        margin = margin(t = 16, b = 10)
      ),
      plot.subtitle = element_markdown(
        size = 10,
        family = "Open Sans",
        margin = margin(b = 16),
        lineheight = 1.3
      ),
      plot.caption = element_text(
        hjust = 0,
        margin = margin(t = 10, b = 6),
        color = "grey35",
        size = 9
      ),
      strip.text = element_text(family = "Open Sans SemiBold"),
      text = element_text(color = "grey25"),
      axis.text = element_text(family = "Inconsolata"),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      panel.grid = element_blank(),
      plot.margin = margin(l = 12, r = 12, b = 6),
      plot.title.position = "plot"
    )
}
theme_set(theme_custom())



## Graphs ==============================================================

colors <- RColorBrewer::brewer.pal(3, "Accent")
colors <- colors[c(1, 2)]

plot_subtitle <- glue::glue("Points show the results in terms of goal difference. Filled shapes for home games.<br>
                   Points left to the dashed line indicate losses, points to the right wins.
                   The errorbars indicate expected<br>goal differences
                   for opponents matching up against teams from the respective tier.<br>
                   Managers: <b style='color:{colors[2]}'>J&uuml;rgen Klopp</b> | <b style='color:{colors[1]}'>Lucien Favre</b>")

plot_caption <- "League tables have been cut into 3 tiers of equal size.
The tier including Borussia Dortmund consists of only 5 teams (in most seasons: tier 1).
The errorbars range from -1 standard deviation from the mean to +1 s.d., thus cover 68 % of the results within the tier.
Mean and s.d. were obtained from all matches played within each tier.
Source: github.com/bydata. Data: dfb.de"

tier_goal_diff %>%
  mutate(tier = paste("Tier", tier)) %>% 
  ggplot(aes(fct_rev(tier))) +
  geom_hline(aes(yintercept = 0), lty = "dashed", col = "grey75", size = 0.5) +
  geom_crossbar(aes(y = -mean, ymin = -perc_16, ymax = -perc_84), 
                  col = "grey60", fill = "grey92", alpha = 0.5, width = 0.3, size = 0.2) +
  geom_jitter(data = bvb_fixtures %>% filter(turf == "away"), aes(fct_rev(paste("Tier", tier)), goal_diff, color = bvb_manager),
              shape = 21, alpha = 1, size = 2, width = 0.3, height = 0.15,
              show.legend = FALSE) +
  geom_jitter(data = bvb_fixtures %>% filter(turf == "home"), aes(fct_rev(paste("Tier", tier)), goal_diff, color = bvb_manager),
              alpha = 0.5, size = 2, width = 0.3, height = 0.1,
              show.legend = FALSE) +
  scale_y_continuous(breaks = seq(-10, 10, 2)) +
  scale_color_brewer(palette = "Accent", type = "qual", aesthetics = c("fill", "color")) +
  coord_flip() +
  labs(title = "Does Favre's Borussia Dortmund underperform\nagainst lower tier opponents?",
       subtitle = plot_subtitle,
       caption = plot_caption,
       y = "Goal difference", x = NULL) +
  facet_wrap(vars(season))

ggsave("plots/bvb_performance_against_tiers_wklopp.png", type = "cairo", dpi = 320, width = 6, height = 6, scale = 1.25)


# only the 2 Favre season so far

plot_subtitle <- glue::glue("Points show the results in terms of goal difference. Filled shapes for home games.<br>
                   Points left to the dashed line indicate losses, points to the right wins.
                   The errorbars indicate expected<br>goal differences
                   for opponents matching up against teams from the respective tier.")

set.seed(123) # random seed for jitter geoms
tier_goal_diff %>%
  filter(season %in% c("2018/2019", "2019/2020")) %>% 
  mutate(tier = paste("Tier", tier)) %>% 
  ggplot(aes(fct_rev(tier))) +
  geom_hline(aes(yintercept = 0), lty = "dashed", col = "grey75", size = 0.5) +
  geom_crossbar(aes(y = -mean, ymin = -perc_16, ymax = -perc_84), 
                col = "grey60", fill = "grey92", alpha = 0.5, width = 0.3, size = 0.2) +
  geom_jitter(data = bvb_fixtures %>% filter(turf == "away", year %in% 2019:2020), aes(fct_rev(paste("Tier", tier)), goal_diff, color = bvb_manager),
              shape = 21, alpha = 1, size = 2, width = 0.3, height = 0.15,
              show.legend = FALSE) +
  geom_jitter(data = bvb_fixtures %>% filter(turf == "home", year %in% 2019:2020), aes(fct_rev(paste("Tier", tier)), goal_diff, color = bvb_manager),
              alpha = 0.5, size = 2, width = 0.3, height = 0.1,
              show.legend = FALSE) +
  # annotation for lower tier teams 2018/2019
  geom_richtext(data = . %>% filter(season == "2018/2019", tier == "Tier 2"), 
                label = "<b style='font-family:Open Sans'>Lost 3 and drew 6</b><br>against 
                <b style='font-family:Open Sans'>lower tier</b><br>teams",
                x = 1.5, y = -5.5, hjust = 0,
                family = "Open Sans Light", size = 2,
                label.color = NA, fill = NA, color = "grey25") +
  # annotation for same tier teams 2018/2019
  geom_richtext(data = . %>% filter(season == "2018/2019", tier == "Tier 1"), 
                label = "<b style='font-family:Open Sans'>Won 9 out of 10</b><br>
                against <b style='font-family:Open Sans'>tier 1</b><br>teams",
                x = 2.5, y = -5.5, hjust = 0,
                family = "Open Sans Light", size = 2,
                label.color = NA, fill = NA, color = "grey25") +
  # annotation for loss against Bayern in 2018/2019
  geom_richtext(data = . %>% filter(season == "2018/2019", tier == "Tier 1"), 
                label = "<i>Ouch!</i>",
                x = 3.3, y = -5.5, hjust = 0,
                family = "Open Sans Light", size = 2,
                label.color = NA, fill = NA, color = "grey25") +
  geom_curve(data = . %>% filter(season == "2018/2019", tier == "Tier 1"), 
             curvature = -0.4, x = 3.3, xend = 2.95, y = -4.4, yend = -4.7,
             col = "grey80", size = 0.15, arrow = arrow(angle = 20, length = unit(0.5, "mm"))) +
  # annotation for lower tier teams 2019/2020
  geom_richtext(data = . %>% filter(season == "2019/2020", tier == "Tier 2"), 
                label = "<b style='font-family:Open Sans'>Lost 2 and drew 4</b><br>against 
                <b style='font-family:Open Sans'>lower tier</b><br>teams",
                x = 1.5, y = -5.5, hjust = 0,
                family = "Open Sans Light", size = 2,
                label.color = NA, fill = NA, color = "grey25") +
  # annotation for same tier teams 2019/2020
  geom_richtext(data = . %>% filter(season == "2019/2020", tier == "Tier 1"), 
                label = "<b style='font-family:Open Sans'>Lost 5 out of 10</b><br>
                against <b style='font-family:Open Sans'>tier 1</b><br>teams",
                x = 2.5, y = -5.5, hjust = 0,
                family = "Open Sans Light", size = 2,
                label.color = NA, fill = NA, color = "grey25") +
  scale_y_continuous(breaks = seq(-10, 10, 2)) +
  scale_color_brewer(palette = "Accent", type = "qual", aesthetics = c("fill", "color")) +
  coord_flip(clip = "off") +
  labs(title = "(Under-)Performance against lower-tier teams\nlikely costed Favre's Dortmund the title in 2018-19",
       subtitle = plot_subtitle,
       caption = plot_caption,
       y = "Goal difference", x = NULL) +
  facet_wrap(vars(season))

ggsave("plots/bvb_performance_against_tiers_favre.png", type = "cairo", dpi = 320, width = 6, height = 4, scale = 1.25)

