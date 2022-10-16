library(tidyverse)
library(ggtext)
library(here)

base_path <- "fcb-saisonstart-2023"
bundesliga_tables_data_path <- here("scrape_bundesliga_tables", "output", "bundesliga_all_tables_df.rds")
bundesliga_tables <- read_rds(bundesliga_tables_data_path)

season_2023 <- bundesliga_tables %>% 
  filter(team == "Borussia Dortmund") %>% 
  filter(start_year == 2023) %>% 
  select(season, position, played, points_3pt) %>%
  add_row(season = "2022/2023", position = NA, played = 8, points_3pt = 15) %>%
  add_row(season = "2022/2023", position = NA, played = 9, points_3pt = 16)

bundesliga_tables %>% 
  filter(team == "Borussia Dortmund") %>% 
  filter(start_year < 2023) %>% 
  select(season, position, played, points_3pt) %>%
  ggplot(aes(played, points_3pt, group = season)) +
  geom_line(color = "grey90", size = 0.15, alpha = 0.75) +
  geom_line(
    data = season_2023,
    color = "#FDE100", size = 1.1
  ) +
  geom_point(
    data = filter(season_2023, played == max(played)),
    color = "#FDE100", size = 2
  ) +
  # best seasons
  ggrepel::geom_text_repel(
    data = . %>% 
      group_by(season) %>% 
      filter(played == max(played)) %>% 
      ungroup() %>% 
      slice_max(order_by = points_3pt, n = 3),
    aes(label = season),
    hjust = 0, color = "grey90", size = 2,
    direction = "y", nudge_x = 0.5
  ) +
  # worst seasons
  ggrepel::geom_text_repel(
    data = . %>% 
      group_by(season) %>% 
      filter(played == max(played)) %>% 
      ungroup() %>% 
      slice_min(order_by = points_3pt, n = 3),
    aes(label = season),
    hjust = 0, color = "grey90", size = 2,
    direction = "y", nudge_x = 0.5
  ) +
  scale_x_continuous(limits = c(1, 40), breaks = c(5, 10, 15, 20, 25, 30, 34, 38)) +
  scale_y_continuous(position = "left") +
  coord_cartesian(clip = "off") +
  labs(
    title = "",
    subtitle = "",
    caption = "Daten: DFB. Visualisierung: Ansgar Wolsing",
    x = "Spieltag", y = "Punkte (3-Punkte-Regel)"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "grey12", fill = "grey12"),
    panel.grid = element_blank(),
    text = element_text(color = "grey80"),
    axis.text = element_text(color = "grey80")
  )
ggsave(here(base_path, "bvb-saisonstart-lines.png"), dpi = 500, width = 8, height = 6)


df_plot <- bundesliga_tables %>% 
  filter(team == "Borussia Dortmund") %>% 
  filter(start_year < 2023) %>% 
  filter(played <= max(season_2023$played)) 

df_plot %>% 
  filter(played == max(season_2023$played)) %>% 
  mutate(compared_to_2023 = case_when(
    points_3pt > season_2023$points_3pt[max(season_2023$played)] ~ "besser",
    points_3pt < season_2023$points_3pt[max(season_2023$played)] ~ "schlechter",
    TRUE ~ "punktgleich"
  )) %>% 
  count(compared_to_2023)

df_plot %>% 
  filter(played == max(season_2023$played)) %>% 
  filter(points_3pt == max(season_2023$points_3pt))

ribbon_summary <- function(x) {
  data.frame(ymin = min(x), ymax = max(x))
}

df_plot %>% 
  select(season, position, played, points_3pt) %>%
  ggplot(aes(played, points_3pt, group = season)) +
  geom_line(color = "grey90", size = 0.15, alpha = 0.6) +
  stat_summary(
    geom = "ribbon",
    fun.data = ribbon_summary,
    aes(x = played, y = points_3pt),
    color = "white", fill = alpha("grey24", 0.8),
    inherit.aes = FALSE
  ) +
  geom_line(data = season_2023, color = "#FDE100", size = 1.4) +
  geom_point(
    data = filter(season_2023, played == max(played)),
    color = "#FDE100", size = 2
  ) +
  geom_text(
    data = filter(season_2023, played == max(played)),
    aes(label = season),
    color = "#FDE100", size = 3, hjust = 0, nudge_x = 0.1, family = "Roboto Condensed"
  ) +
  # best seasons
  ggrepel::geom_text_repel(
    data = . %>% 
      group_by(season) %>% 
      filter(played == max(played)) %>% 
      ungroup() %>% 
      slice_max(order_by = points_3pt, n = 1),
    aes(label = season),
    hjust = 0, color = "grey90", size = 2, box.padding = 0.1,
    direction = "y", nudge_x = 0.1, family = "Roboto Condensed"
  ) +
  # worst seasons
  ggrepel::geom_text_repel(
    data = . %>% 
      group_by(season) %>% 
      filter(played == max(played)) %>% 
      ungroup() %>% 
      slice_min(order_by = points_3pt, n = 2),
    aes(label = season),
    hjust = 0, color = "grey90", size = 2, box.padding = 0.1,
    direction = "y", nudge_x = 0.1, family = "Roboto Condensed"
  ) +
  annotate(
    "label",
    label = c("23 Saisons", "32 Saisons"),
    x = max(season_2023$played) + 0.1,
    y = c(19, 10),
    hjust = 0, family = "Roboto Condensed", color = "grey90", size = 2.5,
    label.size = 0, fill = alpha("grey24", 0.8)
  ) +
  scale_x_continuous(limits = c(1, max(season_2023$played) + .25), 
                     breaks = seq(1, max(season_2023$played), 1)) +
  scale_y_continuous(position = "left", breaks = seq(0, 102, 3)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "<b style='color:#FDE100'>Saisonstart 2022/'23</b>
    von Borussia Dortmund im historischen Vergleich",
    subtitle = "Borussia Dortmund erreicht nach 9 Spieltagen 16 Punkte",
    caption = "Für alle Saisons vor Einführung der 3-Punkte-Regel wurden die Punktzahlen 
    auf die 3-Punkte-Regel umgerechnet.<br>
    Daten: DFB. Visualisierung: Ansgar Wolsing",
    x = "Spieltag", y = "Punkte (3-Punkte-Regel)"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "grey12", fill = "grey12"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey30", size = 0.1),
    text = element_text(color = "grey80"),
    axis.text = element_text(color = "grey80"),
    plot.title = element_markdown(face = "bold", color = "white", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(lineheight = 1.1)
  )
ggsave(here(base_path, "bvb-saisonstart-lines-until-current-matchday-09.png"), dpi = 500, width = 8, height = 6.5)


## Compared to the last 10 seasons --------------

df_plot %>% 
  select(season, position, played, points_3pt) %>%
  filter(season >= "2012/'13") %>% 
  ggplot(aes(played, points_3pt, group = season)) +
  geom_line(color = "grey50", size = 0.15, alpha = 0.6) +
  stat_summary(
    geom = "ribbon",
    fun.data = ribbon_summary,
    aes(x = played, y = points_3pt),
    color = "white", fill = alpha("grey74", 0.8),
    inherit.aes = FALSE
  ) +
  ggfx::with_outer_glow(
    geom_line(data = season_2023, color = "#FDE100", size = 1.4),
    expand = 2, sigma = 1)+
  ggfx::with_outer_glow(
    geom_point(
      data = filter(season_2023, played == max(played)),
      color = "#FDE100", size = 2),
    expand = 2, sigma = 1
  ) +
  geom_label(
    data = filter(season_2023, played == max(played)),
    aes(label = season),
    color = "#FDE100", size = 3, hjust = 0, nudge_x = 0.1, family = "Roboto Condensed",
    label.size = 0, fill = alpha("grey43", 0.6)
  ) +
  # best seasons
  ggrepel::geom_text_repel(
    data = . %>% 
      group_by(season) %>% 
      filter(played == max(played)) %>% 
      ungroup() %>% 
      slice_max(order_by = points_3pt, n = 1),
    aes(label = season),
    hjust = 0, color = "grey30", size = 2, box.padding = 0.1,
    direction = "y", nudge_x = 0.1, family = "Roboto Condensed"
  ) +
  # worst seasons
  ggrepel::geom_text_repel(
    data = . %>% 
      group_by(season) %>% 
      filter(played == max(played)) %>% 
      ungroup() %>% 
      slice_min(order_by = points_3pt, n = 1),
    aes(label = season),
    hjust = 0, color = "grey30", size = 2, box.padding = 0.1,
    direction = "y", nudge_x = 0.1, family = "Roboto Condensed"
  ) +
  scale_x_continuous(limits = c(1, max(season_2023$played) + .5), 
                     breaks = seq(1, max(season_2023$played), 1)) +
  scale_y_continuous(limits = c(0, 3 * max(season_2023$played) + 1),
                     position = "left", breaks = seq(0, 102, 3)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Saisonstart 2022/'23 von Borussia Dortmund",
    subtitle = "Nach 9 Spieltagen, im Vergleich zu den vergangenen 10 Jahren",
    caption = "Daten: DFB. Visualisierung: Ansgar Wolsing",
    x = "Spieltag", y = "Punkte (3-Punkte-Regel)"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "grey90", fill = "grey90"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey30", size = 0.1),
    text = element_text(color = "grey20"),
    axis.text = element_text(color = "grey20"),
    plot.title = element_markdown(face = "bold", color = "black", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(lineheight = 1.1)
  )
ggsave(here(base_path, "bvb-saisonstart-lines-last-decade-until-current-matchday-09.png"), 
       dpi = 300, width = 7, height = 5.5)
