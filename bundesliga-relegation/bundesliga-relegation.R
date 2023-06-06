library(tidyverse)
library(ggtext)
library(here)

base_path <- here("bundesliga-relegation")

bundesliga1 <- read_tsv(here(base_path, "bundesliga-1-relegation.tsv"))
bundesliga2 <- read_tsv(here(base_path, "bundesliga-2-relegation.tsv"))

prepare_df <- function(df, liga){
  df %>% 
    pivot_longer(cols = c(2, 4), names_to = "ligist", values_to = "team") %>% 
    mutate(sieger = str_detect(team, "^\\*"),
           team = str_remove_all(team, "\\*"),
           liga = liga) %>% 
    select(liga, jahr = Datum, ligist, team, sieger)
}

prepare_df(bundesliga1, "1. Bundesliga") %>% 
  bind_rows(prepare_df(bundesliga2, "2. Bundesliga")) %>% 
  filter(sieger) %>% 
  mutate(hoehere_liga = ifelse(liga == "1. Bundesliga" & ligist == "Erstligist" | 
                               liga == "2. Bundesliga" & ligist == "Zweitligist",
                               "Höhere Liga", "Niedrigere Liga"),
         hoehere_liga = factor(hoehere_liga, levels = c("Niedrigere Liga", "Höhere Liga")),
         liga = factor(liga, levels = c("2. Bundesliga", "1. Bundesliga"))) %>% 
  count(liga, hoehere_liga, ligist) %>% 
  arrange(liga, desc(hoehere_liga)) %>% 
  group_by(liga) %>% 
  mutate(label_ypos = (lag(n, 1, default = 0) + n / 2) / sum(n)) %>% #View()
  ggplot(aes(liga, n, fill = hoehere_liga, group = hoehere_liga)) + 
  geom_col(position = "fill", width = 0.8) +
  geom_richtext(
    aes(label = sprintf("%s<br>**%d**", ligist, n), y = label_ypos),
    family = "Roboto Condensed", color = "white", label.size = 0, fill = NA,
    show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Redon")) +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_flip() +
  labs(
    title = "Relegation: Für Zweitligisten wenig zu gewinnen",
    subtitle = "Sieger der Relegationen seit 2008/'09",
    x = NULL,
    y = "Anteil gewonnene Relegationsduelle",
    fill = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(here(base_path, "bundesliga-relegation.png"), width = 6, height = 4, scale = 1.1)
