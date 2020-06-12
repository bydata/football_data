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

final_tables_reduced <- final_tables %>% 
  # Faktor für Meister, Vizemeister, 1. Nichtabstiegsplatz (ohne Relegationsplatz), andere
  mutate(position_fct = case_when(
    position == 1 ~ "Meister",
    position == 2 ~ "Vize",
    start_year <= 1964 & position == 14 
    | start_year %in% 1965:1973 & position == 16 
    | start_year >= 1974 & position == 15 ~ "Garantierter Klassenerhalt",
    TRUE ~ "Andere"
  ) %>% factor(levels = c("Meister", "Vize", "Garantierter Klassenerhalt"))) %>% 
  filter(position_fct != "Andere") %>% 
  group_by(position_fct) %>% 
  mutate(
    points_mavg = rollmean(points_3pt, k = 5, align = "right", fill = NA),
    goal_diff_mavg = rollmean(goal_diff, k = 5, align = "right", fill = NA)
         ) %>% 
  ungroup() %>% 
  select(season, start_year, position_fct, points_mavg, points, points_3pt, goal_diff, goal_diff_mavg)

# bestes Punkteergebnis des Tabellen-1. nach Jahrzehnt
final_tables %>% 
  filter(position == 1) %>% 
  arrange(-points_3pt) %>% 
  mutate(decade = 10 * start_year %/% 10 ) %>% 
  select(season, decade, points_3pt)

# Punkte: 5 Jahre gleitender Durchschnitt
final_tables_reduced %>% 
  ggplot(aes(start_year, points_mavg)) + 
  annotate("rect", xmin = 2010, xmax = 2020, ymin = 70, ymax = 95, fill = "grey97") +
  geom_line(aes(y = points_mavg, col = position_fct), size = 1.2) +
  geom_line(data = . %>% filter(position_fct == "Meister"), aes(y = points_3pt), size = 0.6, lty = "dashed") +
  annotate("richtext", x = 2020, y = 100, size = 2, hjust = 1,
           label = "In den Top 10 der Meister mit den meisten Punkten<br>sind die 2010er Jahre 7 Mal vertreten", 
           label.color = NA, color = "grey25") +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_color_manual(values = c("Meister" = "#11BB21", "Vize" = "grey40", 
                                "Garantierter Klassenerhalt" = "grey70", "1 (Jahreswerte)" = "grey25")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Bayern-Dominanz in den 2010er-Jahren",
       subtitle = "Punkte des Meisters, Vizemeisters und des ersten Nichtabsteigers",
       caption = "5 Jahre gleitender Mittelwert. 3-Punkte-Regel für alle Jahre angenommen.\nGarantierter Klassenerhalt: Bis 64/65 14. Platz, 65/66-73/74: 16. Platz, 
       ab 74/75: 15. Platz (Relegationsplatz gilt als potentieller Abstiegsplatz).\nQuelle: @4nsgarW. Daten: dfb.de",
       x = NULL, y = "Torverhältnis", col = NULL) +
  custom_theme +
  theme(legend.position = "top", legend.justification = "left")

ggsave("plots/Bundesliga_Punkte_Zeitverlauf.png", dpi = 600, type = "cairo", width = 8, height = 5.7)

  
# bestes Torverhältnis des Tabellen-1. nach Jahrzehnt
final_tables %>% 
  filter(position == 1) %>% 
  arrange(-goal_diff) %>% 
  mutate(decade = 10 * start_year %/% 10) %>% 
  select(season, decade, goal_diff)

# 5 Jahre gleitender Durchschnitt
final_tables_reduced %>% 
  ggplot(aes(start_year)) + 
  annotate("rect", xmin = 2010, xmax = 2020, ymin = 36, ymax = 82, fill = "grey97") +
  geom_line(aes(y = goal_diff_mavg, col = position_fct), size = 1.2) +
  geom_line(data = . %>% filter(position_fct == "Meister"), aes(y = goal_diff), size = 0.6, lty = "dashed") +
  annotate("richtext", x = 2020, y = 90, size = 2, hjust = 1,
           label = "In den Top 10 der Meister mit bestem Torverhältnis<br>sind die 2010er Jahre 8 Mal vertreten", 
           label.color = NA, color = "grey25") +
  scale_x_continuous(breaks = seq(1970, 2020, 10)) +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_color_manual(values = c("Meister" = "#11BB21", "Vize" = "grey40", 
                                "Garantierter Klassenerhalt" = "grey70", "1 (Jahreswerte)" = "grey25")) +
  coord_cartesian(ylim = c(-40, 90)) +
  labs(title = "Bayern-Dominanz in den 2010er-Jahren",
       subtitle = "Torverhältnis des Meisters, Vizemeisters und ersten Nichtabsteigers",
       caption = "5 Jahre gleitender Mittelwert\nGarantierter Klassenerhalt: Bis 64/65 14. Platz, 65/66-73/74: 16. Platz, 
       ab 74/75: 15. Platz (Relegationsplatz gilt als potentieller Abstiegsplatz).\nQuelle: @4nsgarW. Daten: dfb.de",
       x = NULL, y = "Torverhältnis", col = NULL) +
  custom_theme +
  theme(legend.position = "top", legend.justification = "left")

ggsave("plots/Bundesliga_Torverhältnis_Zeitverlauf.png", dpi = 600, type = "cairo", width = 8, height = 5.7)



# Anzahl verschiedener Meister und Vizemeister nach Jahrzehnt
final_tables %>% 
  filter(position <= 2) %>%
  filter(start_year >= 1970) %>% 
  mutate(decade = 10 * start_year %/% 10) %>% 
  group_by(decade, position) %>% 
  summarize(n_distinct = n_distinct(team)) %>% 
  ungroup() %>% 
  ggplot(aes(decade, n_distinct)) +
  geom_col(aes(fill = factor(position)), position = "dodge") +
  custom_theme




# in wievielen anderen Saisons wäre der Vizemeister Meister geworden?
champion_points <- final_tables %>% 
  filter(position == 1) %>% 
  select(season, start_year, points_3pt) %>% 
  # Dummy Group hinzufügen um Cross Join zu ermöglichen
  mutate(dummy_group = 1)


final_tables %>% 
  filter(position == 2) %>%
  mutate(dummy_group = 1) %>% 
  full_join(champion_points, by = "dummy_group", suffix = c(".2nd", ".1st")) %>% 
  # nur Fälle behalten, wo Punkte des Vizes >= Punkte des Meisters
  filter(points_3pt.2nd > points_3pt.1st) %>% 
  # alle Fälle außer der aktuellen Saison behalten
  filter(start_year.1st != start_year.2nd) %>% 
  group_by(season.2nd, start_year.2nd, team) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  complete(start_year.2nd = 1963:2018) %>% 
  mutate(n = replace_na(n, 0)) %>%
  # anteilig zu Vorjahren
  mutate(rel = n / (2018 - 1963)) %>% 
  mutate(rel_mavg = rollmean(rel, k = 5, align = "right", fill = NA)) %>% 
  ggplot(aes(start_year.2nd, rel)) +
  geom_col(aes(fill = (rel > 0.5), alpha = (rel > 0.5)), show.legend = FALSE) +
  geom_line(aes(y = rel_mavg, col = "Gleitender Durchschnitt (5 Jahre)"), size = 1.2) +
  geom_text(data = . %>% filter(rel > 0.5), 
            aes(label = sprintf("%s (%s)", team, season.2nd)), 
            vjust = 0.5,
            size = 2, family = "CenturyGothic", angle = 90) +
  #geom_smooth(span = 0.5, se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("TRUE" = "#11BB21", "FALSE" = "grey70")) +
  scale_alpha_discrete(range = c(0.1, 0.25)) +
  scale_color_manual(values = c("grey40")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Zur falschen Zeit am falschen Ort",
       subtitle = "In den 2010er-Jahren wären 4 Bundesliga-Vizemeister in mehr als 50% der anderen Saisons\nMeister geworden.",
       caption = "3-Punkte-Regel für alle Jahre angenommen. Quelle: @4nsgarW. Daten: dfb.de",
       x = NULL, y = "Anteil Saisons", col = NULL) +
  custom_theme +
  theme(legend.position = "top", legend.justification = "left")

ggsave("plots/bundesliga_vize_als_meister.png", type = "cairo", dpi = 600, width = 8, height = 5.7)



# Titelverteidigungen
final_tables %>% 
  filter(position == 1) %>% 
  select(season, start_year, team) %>% 
  mutate(champ_prev = lag(team),
         champ_next = lead(team),
         title_defended = team == champ_prev,
         title_defended_count = ifelse(title_defended, 1, 0),
         title_defended_count = ifelse(title_defended, lag(title_defended_count) + 1, 0)
         ) %>% 
  filter(!is.na(champ_prev)) %>% 
  ggplot(aes(start_year, y = 1 * title_defended)) +
  geom_col(aes()) +
  geom_text(data = . %>% filter(title_defended), aes(label = team), y = 0.5, size = 2, angle = 90) +
  theme_void()





