library(tidyverse)
library(ggtext)
library(here)

base_path <- "fcb-saisonstart-2023"
bundesliga_tables_data_path <- here("scrape_bundesliga_tables", "output", "bundesliga_all_tables_df.rds")
bundesliga_tables <- read_rds(bundesliga_tables_data_path)

season_2023 <- tibble(
  season = "2022/2023",
  position = c(1, 1, 1, 1, 3, 3, 4),
  played = 1:7,
  points_3pt = c(3, 6, 9, 10, 11, 12, 12)
)

bundesliga_tables %>% 
  filter(team == "Bayern München") %>% 
  filter(start_year < 2023) %>% 
  select(season, position, played, points_3pt) %>%
  ggplot(aes(played, points_3pt, group = season)) +
  geom_line(color = "grey90", size = 0.15, alpha = 0.75) +
  geom_line(
    data = season_2023,
    color = "#9D8DF1", size = 1.1
  ) +
  geom_point(
    data = filter(season_2023, played == max(played)),
    color = "#9D8DF1", size = 2
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
ggsave(here(base_path, "fcb-saisonstart-lines.png"), dpi = 500, width = 8, height = 6)


df_plot <- bundesliga_tables %>% 
  filter(team == "Bayern München") %>% 
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
  filter(points_3pt == 12)

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
  geom_line(data = season_2023, color = "#9D8DF1", size = 1.4) +
  geom_point(
    data = filter(season_2023, played == max(played)),
    color = "#9D8DF1", size = 2
  ) +
  geom_text(
    data = filter(season_2023, played == max(played)),
    aes(label = season),
    color = "#9D8DF1", size = 3, hjust = 0, nudge_x = 0.1, family = "Roboto Condensed"
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
    label = c("47 Saisons", "10 Saisons"),
    x = max(season_2023$played) + 0.1,
    y = c(16, 10),
    hjust = 0, family = "Roboto Condensed", color = "grey90", size = 2.5,
    label.size = 0, fill = alpha("grey24", 0.8)
  ) +
  scale_x_continuous(limits = c(1, 7.25), breaks = seq(1, 7, 1)) +
  scale_y_continuous(position = "left", breaks = seq(0, 102, 3)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Vom bestem zu einem der schlechteren Saisonstarts in nur 4 Spieltagen",
    subtitle = "Der FC Bayern erreicht nach 7 Spieltagen der
    <b style='color:#9D8DF1'>Saison 2022/'23</b>
    lediglich 12 Punkte",
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
    plot.title = element_text(face = "bold", color = "white", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown()
  )
ggsave(here(base_path, "fcb-saisonstart-lines-until-current-matchday.png"), dpi = 500, width = 8, height = 6.5)


## Season results after a "bad" start

bad_start_final_results <- df_plot %>% 
  filter(played == max(season_2023$played)) %>% 
  filter(points_3pt <= 12) %>% 
  inner_join(subset(bundesliga_tables, played == 34  & season != "1991/1992" | 
                      played == 38 & season == "1991/1992"),
             by = c("team", "season"), suffix = c(".start", ".final")) %>% 
  select(season, start_year.start, team, played.start, points_3pt.start, position.start,
         points_3pt.final, position.final) %>% 
  arrange(position.final) %>% 
  mutate(position_cat.final = case_when(
    position.final == 1 ~ "Meister",
    position.final <= 4 ~ "Platz 2-4",
    position.final <= 9 ~ "Platz 5-9",
    position.final <= 12 ~ "Platz 10-12",
    TRUE ~ "Platz 13-18"
  ),
  position_cat.final = fct_inorder(position_cat.final)
  )
bad_start_final_results

bad_start_final_results  %>% 
  ggplot(aes(position_cat.final)) +
  geom_bar() +
  coord_cartesian()


bad_start_final_results  %>%
  mutate(position_cat.final = fct_rev(position_cat.final)) %>% 
  count(position_cat.final) %>% 
  mutate(share = n / sum(n)) %>% 
  ggplot(aes(x = 1, y = share, fill = position_cat.final, group = position_cat.final)) +
  geom_col(position = "stack") +
  geom_richtext(
    aes(y = share, 
        label = paste0(position_cat.final, "<br>", "**", n, "x", "**")),
    position = position_stack(vjust = 0.5), hjust = 0.5,
    family = "Roboto Condensed",
    label.size = 0, fill = alpha("white", 0.6)
  ) +
  scale_x_continuous(limits = c(0, 1.75)) +
  scale_fill_manual(values = c(colorspace::desaturate("#0066B2", 0.8), 
                               colorspace::desaturate("#0066B2", 0.5),  
                               "#0066B2", "#DC052D")) +
  coord_flip() +
  guides(
    # fill = guide_legend(reverse = TRUE)
    fill = "none") +
  labs(
    title = "Nach schlechtem Saisonstart 4 Mal Meister geworden",
    subtitle = "Der FC Bayern ist mit 12 Punkten aus 7 Spielen in die Saison 2022/'23 gestartet.
    10 Mal startete der Verein noch schlechter. Die Grafik zeigt die Endplatzierung der Bayern in diesen Saisons.",
    caption = "Daten: DFB. Visualisierung: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "top",
    text = element_text(color = "grey30"),
    plot.title = element_text(color = "grey2", face = "bold", size = 16),
    plot.subtitle = element_textbox(width = 0.9, lineheight = 1.1),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(here(base_path, "fcb-saisonstart-lines-final_result.png"), width = 8, height = 4)


df_plot %>% 
  filter(played == max(season_2023$played)) %>% 
  mutate(bad_start = ifelse(points_3pt <= 12, "12 Punkte oder weniger nach 7 Spielen",
                            "13 oder mehr Punkte nach 7 Spielen"),
         bad_start = factor(bad_start),
         bad_start = fct_rev(bad_start)) %>% 
  inner_join(subset(bundesliga_tables, played == 34  & season != "1991/1992" | 
                      played == 38 & season == "1991/1992"),
             by = c("team", "season"), suffix = c(".start", ".final")) %>% 
  select(season, start_year.start, team, played.start, points_3pt.start, position.start,
         points_3pt.final, position.final, bad_start) %>% 
  arrange(position.final) %>% 
  mutate(position_cat.final = case_when(
    position.final == 1 ~ "Meister",
    position.final <= 4 ~ "Platz 2-4",
    position.final <= 9 ~ "Platz 5-9",
    position.final <= 12 ~ "Platz 10-12",
    TRUE ~ "Platz 13-18"
  ),
  position_cat.final = fct_inorder(position_cat.final),
  position_cat.final = fct_rev(position_cat.final)
  )  %>%
  count(bad_start, position_cat.final) %>% 
  group_by(bad_start) %>% 
  mutate(share = n / sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = bad_start, y = share, fill = position_cat.final, group = position_cat.final)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(
    data = ~distinct(., bad_start),
    aes(label = bad_start, x = as.numeric(bad_start) + 0.4, y = 0),
    inherit.aes = FALSE, stat = "unique", hjust = 0, family = "Roboto Condensed", 
    color = "grey30"
  ) +
  geom_richtext(
    aes(y = share, 
        label = paste0(position_cat.final, "<br>", "**", n, "x", "**")),
    position = position_stack(vjust = 0.5), hjust = 0.5,
    family = "Roboto Condensed",
    label.size = 0, fill = alpha("white", 0.6)
  ) +
  scale_fill_manual(values = c("grey70", "grey50", "#0066B2", "#DC052D")) +
  coord_flip() +
  guides(fill = "none") +
  labs(
    title = "Nach schlechtem Saisonstart 4 Mal Meister geworden",
    subtitle = "Der FC Bayern ist mit 12 Punkten aus 7 Spielen in die Saison 2022/'23 gestartet.
    10 Mal startete der Verein noch schlechter. Die Grafik zeigt die Endplatzierung der Bayern in diesen Saisons.",
    caption = "Daten: DFB. Visualisierung: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "top",
    text = element_text(color = "grey30"),
    plot.title = element_text(color = "grey2", face = "bold", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.9, lineheight = 1.1),
    plot.margin = margin(6, 6, 6, 6),
    axis.text = element_blank()
  )
ggsave(here(base_path, "fcb-saisonstart-lines-final_result.png"), width = 7, height = 4)

