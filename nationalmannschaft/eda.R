library(tidyverse)
library(ggtext)

base_path <- "nationalmannschaft"

bundestrainer <- read_tsv(here(base_path, "data", "bundestrainer.tsv"))
bundestrainer <- bundestrainer %>% 
  mutate(across(c(coach_from, coach_to), dmy),
         coach_to = replace_na(coach_to, today()))

results %>% 
  count(match_date, sort = TRUE)

results <- results %>% 
  select(-match_id) %>% 
  dis

# results %>% 
#   right_join(bundestrainer, by = join_by(match_date, between(match_date, coach_from, coach_to)))

df_plot <- results %>% 
  cross_join(bundestrainer) %>% 
  filter(match_date >= coach_from, match_date <= coach_to) %>% 
  arrange(match_date) %>% 
  mutate(
    de_pts = case_when(
      home_team %in% c("BRD", "Deutschland") & home_goals > away_goals ~ 3,
      home_team %in% c("BRD", "Deutschland") & home_goals < away_goals ~ 0,
      away_team %in% c("BRD", "Deutschland") & away_goals > home_goals ~ 3,
      away_team %in% c("BRD", "Deutschland") & away_goals < home_goals ~ 0,
      TRUE ~ 1
    )
  ) %>%
  group_by(coach_name) %>% 
  mutate(de_pts_cumul = cumsum(de_pts)) %>% 
  ungroup() 


df_plot %>% 
  filter(coach_name != "kein Trainer") %>% 
  ggplot(aes(match_date, de_pts_cumul, col = coach_name)) +
  geom_step() +
  # add WC finals
  geom_point(
    data = ~subset(., match_date %in% as_date(c("1954-07-04", "1974-07-07", 
                                                "1990-07-08", "2014-07-13"))),
    aes(col = coach_name),
    size = 1.5
  ) +
  geom_text(
    data = ~group_by(., coach_name) %>% 
      filter(match_date == max(match_date)),
    aes(label = coach_name),
    angle = 90, size = 3, hjust = 0, nudge_y = 5, family = "Cabinet Grotesk Medium"
  ) +
  colorspace::scale_color_discrete_qualitative("Cold") +
  guides(color = "none") + 
  theme_minimal(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = "white", fill = "white")
  )
ggsave(here(base_path, "plots", "bundestrainer-points-cumul.png"), width = 6, height = 4)


df_plot %>% 
  mutate(days_active = match_date - coach_from,
         days_active = as.numeric(days_active)) %>% 
  ggplot(aes(days_active, de_pts_cumul, col = coach_name)) +
  geom_step() +
  facet_wrap(vars(coach_name))


df_plot %>% 
  count(coach_name, sort = TRUE)

df_plot %>% 
  mutate(days_active = match_date - coach_from,
         days_active = as.numeric(days_active)) %>% 
  group_by(coach_name) %>% 
  mutate(match_coach_id = row_number()) %>% 
  ungroup() %>% 
  filter(match_coach_id <= 24, coach_name != "kein Trainer") %>% 
  ggplot(aes(match_coach_id, de_pts_cumul, group = coach_name)) +
  geom_line(aes(col = coach_name == "Hansi Flick"), show.legend = FALSE, 
            linewidth = 0.33) +
  geom_line(
    data = ~subset(., coach_name == "Hansi Flick"), col = "#642CA9", linewidth = 0.8) +
  geom_point(
    data = ~subset(., coach_name == "Hansi Flick" & match_coach_id == max(match_coach_id)), 
    col = "#642CA9", size = 1) +
  geom_text(
    data = ~group_by(., coach_name) %>% 
      filter(match_coach_id == max(match_coach_id)) %>% 
      ungroup(),
    aes(label = sprintf("%s (%d-%s)", coach_name, year(coach_from), 
                        ifelse(coach_name == "Hansi Flick", "", str_sub(year(coach_to), 3, 4))),
        col = coach_name == "Hansi Flick",
        y = de_pts_cumul + ifelse(coach_name %in% c("Berti Vogts", "Hansi Flick", "Otto Nerz"), -1, 0)),
        # y = de_pts_cumul),
    size = 1.75, hjust = 0, nudge_x = 0.15, show.legend = FALSE,
    family = "Cabinet Grotesk Medium"
  ) +
  scale_y_continuous(expand = expansion(add = c(0, 3))) +
  scale_color_manual(values = c("grey60", "#642CA9")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "<span style='color:#642CA9'>Flicks</span> mit perfektem Start, seitdem durchwachsen",
    subtitle = "Auf alle Spiele wurde die 3-Punkte-Regel angewandt",
    caption = "Quelle: DFB. Visualisierung: Ansgar Wolsing",
    x = "Anzahl Spiele als Bundestrainer",
    y = "Anzahl Punkte (3-Punkte-Regel)"
  ) +
  theme_minimal(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(t = 2, l = 2, b = 2, r = 60),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    text = element_text(color = "grey30"),
    plot.title = element_markdown(color = "grey2", family = "Cabinet Grotesk", face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 1)
  )
ggsave(here(base_path, "plots", "bundestrainer-points-first-matches.png"), width = 6, height = 4)

df_plot %>% 
  mutate(days_active = match_date - coach_from,
         days_active = as.numeric(days_active)) %>% 
  group_by(coach_name) %>% 
  mutate(match_coach_id = row_number()) %>% 
  ungroup() %>% 
  filter(match_coach_id <= 25, coach_name != "kein Trainer") %>% 
  ggplot(aes(match_coach_id, de_pts_cumul, group = coach_name)) +
  geom_line(aes(col = coach_name == "Hansi Flick"), show.legend = FALSE, 
            linewidth = 0.2) +
  geom_line(
    data = ~subset(., coach_name == "Hansi Flick"), col = "#642CA9", linewidth = 1) +
  geom_point(
    data = ~subset(., coach_name == "Hansi Flick" & match_coach_id == max(match_coach_id)), 
    col = "#642CA9", size = 1) +
  geom_text(
    data = ~group_by(., coach_name) %>% 
      filter(match_coach_id == max(match_coach_id)) %>% 
      ungroup(),
    aes(label = sprintf("%s (%d-%s)", coach_name, year(coach_from), 
                        ifelse(coach_name == "Hansi Flick", "", str_sub(year(coach_to), 3, 4))),
        col = coach_name == "Hansi Flick",
        fontface = ifelse(coach_name == "Hansi Flick", "bold", "plain"),
        size = ifelse(coach_name == "Hansi Flick", 2.25, 1.5),
        y = de_pts_cumul + ifelse(coach_name %in% c("Berti Vogts"), 1.5, 0)),
    hjust = 0, nudge_x = 0.15, show.legend = FALSE,
    family = "Source Sans Pro"
  ) +
  scale_x_continuous(expand = expansion(add = c(0.2, 1))) +
  scale_y_continuous(expand = expansion(add = c(0, 3))) +
  scale_color_manual(values = c("grey58", "#642CA9")) +
  scale_size_identity() +
  coord_cartesian(clip = "off") +
  labs(
    title = "<span style='color:#642CA9'>Flicks</span> perfekter Start, bitteres Ende?",
    subtitle = "Vergleich der DFB-Bundestrainer in den jeweils ersten 25 Länderspielen.<br>
    Auf alle Spiele wurde die 3-Punkte-Regel angewandt",
    caption = "Quelle: DFB. Visualisierung: Ansgar Wolsing",
    x = "Anzahl Spiele als Bundestrainer",
    y = "Anzahl Punkte (3-Punkte-Regel)"
  ) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(t = 2, l = 2, b = 2, r = 60),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    text = element_text(color = "grey32"),
    axis.text = element_text(color = "grey32"),
    plot.title = element_markdown(
      color = "black", family = "Source Sans Pro SemiBold", size = 16,
      margin = margin(t = 2, b = 4)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(lineheight = 1.1),
    plot.caption = element_text(hjust = 1)
  )
ggsave(here(base_path, "plots", "bundestrainer-points-25-matches.png"), width = 6, height = 4)
