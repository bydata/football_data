library(tidyverse)
library(ggforce)
library(ggtext)
library(zoo)

# graphs setup
source("R/02_graphs_setup.R")

# load data (scraped from dfb.de)
final_tables <- read_csv("input/bundesliga_final_tables.csv")
glimpse(final_tables)

# wie viele Mannschaften je Saison?
count(final_tables, season) %>% 
  count(n)

final_tables %>% 
  filter(position %in% c(1,2, 16)) %>% 
  ggplot(aes(start_year, points_3pt)) + 
  geom_line(aes(col = factor(position)), size = 1) +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  coord_cartesian(ylim = c(0, 100)) +
  custom_theme

# bestes Punkteergebnis des Tabellen-1. nach Jahrzehnt
final_tables %>% 
  filter(position == 1) %>% 
  arrange(-points_3pt) %>% 
  mutate(decade = 10 * start_year %/% 10 ) %>% 
  select(season, decade, points_3pt)

# Punkte: 5 Jahre gleitender Durchschnitt
final_tables %>% 
  filter(position %in% c(1, 2, 16)) %>% 
  group_by(position) %>% 
  mutate(points_mavg = rollmean(points_3pt, k = 5, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  ggplot(aes(start_year, points_mavg)) + 
  annotate("rect", xmin = 2010, xmax = 2020, ymin = 70, ymax = 95, fill = "grey97") +
  geom_line(aes(y = points_mavg, col = factor(position)), size = 1.2) +
  geom_line(data = . %>% filter(position == 1), aes(y = points_3pt), size = 0.6, lty = "dashed") +
  annotate("richtext", x = 2020, y = 100, size = 2, hjust = 1,
           label = "In den Top 10 der Meister mit den meisten Punkten<br>sind die 2010er Jahre 7 Mal vertreten", 
           label.color = NA, color = "grey25") +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_color_manual(values = c("1" = "#11BB21", "2" = "grey40", 
                                "16" = "grey70", "1 (Jahreswerte)" = "grey25")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Bayern-Dominanz in den 2010er-Jahren",
       subtitle = "Punkte des Meisters, Vizemeisters und Tabellen-16.",
       caption = "5 Jahre gleitender Mittelwert. 3-Punkte-Regel für alle Jahre angenommen.\nQuelle: @4nsgarW. Daten: dfb.de",
       x = NULL, y = "Torverhältnis", col = "Endplatzierung") +
  custom_theme +
  theme(legend.position = "top", legend.justification = "left")

ggsave("plots/Bundesliga_Punkte_Zeitverlauf.png", dpi = 600, type = "cairo", width = 8, height = 5.7)

  
# bestes Torverhältnis des Tabellen-1. nach Jahrzehnt
final_tables %>% 
  filter(position == 1) %>% 
  arrange(-goal_diff) %>% 
  mutate(decade = 10 * start_year %/% 10 ) %>% 
  select(season, decade, goal_diff)

# 5 Jahre gleitender Durchschnitt
final_tables %>% 
  filter(position %in% c(1, 2, 16)) %>% 
  group_by(position) %>% 
  mutate(goal_diff_mavg = rollmean(goal_diff, k = 5, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  #filter(!is.na(goal_diff_mavg)) %>% 
  ggplot(aes(start_year)) + 
  annotate("rect", xmin = 2010, xmax = 2020, ymin = 36, ymax = 82, fill = "grey97") +
  geom_line(aes(y = goal_diff_mavg, col = factor(position)), size = 1.2) +
  geom_line(data = . %>% filter(position == 1), aes(y = goal_diff), size = 0.6, lty = "dashed") +
  annotate("richtext", x = 2020, y = 90, size = 2, hjust = 1,
           label = "In den Top 10 der Meister mit bestem Torverhältnis<br>sind die 2010er Jahre 8 Mal vertreten", 
           label.color = NA, color = "grey25") +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_color_manual(values = c("1" = "#11BB21", "2" = "grey40", 
                                "16" = "grey70", "1 (Jahreswerte)" = "grey25")) +
  coord_cartesian(ylim = c(-40, 90)) +
  labs(title = "Bayern-Dominanz in den 2010er-Jahren",
       subtitle = "Torverhältnis des Meisters, Vizemeisters und Tabellen-16.",
       caption = "5 Jahre gleitender Mittelwert\nQuelle: @4nsgarW. Daten: dfb.de",
       x = NULL, y = "Torverhältnis", col = "Endplatzierung") +
  custom_theme +
  theme(legend.position = "top", legend.justification = "left")

ggsave("plots/Bundesliga_Torverhältnis_Zeitverlauf.png", dpi = 600, type = "cairo", width = 8, height = 5.7)

