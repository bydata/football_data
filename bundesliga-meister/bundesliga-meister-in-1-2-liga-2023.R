library(tidyverse)
library(ggtext)
library(here)
# devtools::install_github("hrbrmstr/waffle")
library(waffle)


base_path <- "bundesliga-meister"

df <- read_tsv(here(base_path, "meister-1-2-bundesliga-2023.tsv"),
               name_repair = janitor::make_clean_names)

# Custom theme
theme_set(
  theme_minimal(base_family = "Source Sans Pro") +
    theme(
      plot.background = element_rect(color = "grey97", fill = "grey97"),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.2, color = "grey50",
                                        linetype = "dotted"),
      plot.title = element_markdown(
        family = "Source Sans Pro SemiBold", lineheight = 1.2),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(width = 0.9, lineheight = 1),
      plot.caption = element_markdown(hjust = 0),
      strip.text = element_text(family = "Source Sans Pro SemiBold", size = 10),
      text = element_text(color = "grey30"),
      axis.text = element_text(color = "grey30"),
      axis.text.x = element_text(family = "Source Sans Pro SemiBold", size = 10)
    )
)


df %>% 
  filter(jemals_meister) %>% 
  ggplot(aes(liga)) +
  geom_bar(width = 0.6, fill = "#296EB4", alpha = 0.9, 
           col = "white", linewidth = 0.2) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count", vjust = 1.75, col = "white",
    family = "Chivo", size = 4
  ) +
  labs(
    title = "Mehr Deutsche Meister in der 2. Bundesliga",
    subtitle = "Anzahl Mannschaften, die mindestens einen Meistertitel gewonnen haben",
    caption = "Quelle: Wikipedia. Visualisierung: Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )
ggsave(here(base_path, "anzahl_meister.png"), width = 6, height = 4)



df %>% 
  filter(jemals_meister) %>% 
  count(liga, wt = meisterschaften, name = "anzahl_titel") %>% 
  ggplot(aes(liga, anzahl_titel)) +
  geom_col(width = 0.6, fill = "#296EB4", alpha = 0.9, col = "white", linewidth = 0.2) +
  geom_text(
    aes(label = anzahl_titel),
    vjust = 1.75, col = "white",
    family = "Chivo", size = 4
  ) +
  labs(
    title = "Mehr Meistertitel in der 1. Bundesliga",
    subtitle = "Gesamtzahl der Titel je Liga",
    caption = "Quelle: Wikipedia. Visualisierung: Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )
ggsave(here(base_path, "summe_meister.png"), width = 6, height = 4)


df %>% 
  filter(jemals_meister) %>% 
  mutate(
    liga_fcb = ifelse(team == "FC Bayern München", team, liga),
    liga_fcb = factor(liga_fcb, levels = c("FC Bayern München", "1. Bundesliga", "2. Bundesliga"))
         ) %>% 
  count(liga, liga_fcb, wt = meisterschaften, name = "anzahl_titel") %>% 
  ggplot(aes(liga, group = liga_fcb, anzahl_titel)) +
  geom_col(
    aes(fill = liga_fcb),
    width = 0.6, alpha = 0.9, position = "dodge",
    col = "white", linewidth = 0.2) +
  geom_label(
    aes(label = anzahl_titel),
    vjust = 1.75, col = "white", fill = alpha("white", 0.3), label.size = 0,
    family = "Chivo", size = 4, position = position_dodge(width = 0.6)
  ) +
  geom_text(
    data = ~subset(., liga == "1. Bundesliga"),
    aes(label = c("Bayern München", "Rest der Liga"),
        y = 2),
    vjust = 0.25, hjust = 0, col = "white", angle = 90,
    family = "Source Sans Pro", size = 4, position = position_dodge(width = 0.6)
  ) +
  scale_fill_manual(values = c("#1789FC", "#296EB4","#296EB4")) +
  guides(fill = "none") + 
  labs(
    title = "<span style='color:#1789FC'>Bayern München</span> hat mehr Titel 
    als der Rest der 1. Liga,<br>
    und die Zweitligisten haben mehr Titel als die übrigen Erstligisten",
    subtitle = "Gesamtzahl der Titel",
    caption = "Quelle: Wikipedia. Visualisierung: Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )
ggsave(here(base_path, "anzahl_meister-fcb-separat.png"), width = 6, height = 4)




df %>% 
  filter(jemals_meister) %>% 
  mutate(davon_oberliga = meisterschaften - davon_meisterschaften_ddr - davon_bundesliga) %>% 
  pivot_longer(cols = c(davon_oberliga, davon_bundesliga, davon_meisterschaften_ddr),
               names_to = "ligasystem", values_to = "n_titel") %>% 
  mutate(ligasystem = case_match(
    ligasystem,
    "davon_bundesliga" ~ "Bundesliga",
    "davon_meisterschaften_ddr" ~ "DDR-Meister",
    "davon_oberliga" ~ "Vor Bundesliga (inkl. BRD bis 1963)"
  ),
  ligasystem = factor(
    ligasystem, 
    levels = c("Bundesliga", "DDR-Meister", "Vor Bundesliga (inkl. BRD bis 1963)"))
  ) %>% 
  count(liga, ligasystem, wt = n_titel, name = "n_titel") %>% 
  ggplot(aes(fill = ligasystem, values = n_titel)) +
  geom_waffle(na.rm = TRUE, radius = unit(0.2, "npc"), col = "white", size = 0.8,
              linewidth = 0.2) +
  scale_fill_manual(values = c("#296EB4", "#FDB833", "#89B6A5")) +
  coord_equal() +
  facet_wrap(vars(liga)) +
  labs(
    title = "Großteil ihrer Meistertitel gewannen die Zweitligisten<br>
    vor Einführung der Bundesliga",
    caption = "Quelle: Wikipedia. Visualisierung: Ansgar Wolsing",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme(
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_markdown(hjust = 0.5),
    plot.subtitle = element_textbox(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5)
  )
ggsave(here(base_path, "anzahl_meister-ligasystem-waffle.png"), width = 6, height = 4)


df %>% 
  filter(jemals_meister) %>% 
  mutate(davon_oberliga = meisterschaften - davon_meisterschaften_ddr - davon_bundesliga) %>% 
  pivot_longer(cols = c(davon_oberliga, davon_bundesliga, davon_meisterschaften_ddr),
               names_to = "ligasystem", values_to = "n_titel") %>% 
  mutate(ligasystem = case_match(
    ligasystem,
    "davon_bundesliga" ~ "Bundesliga",
    "davon_meisterschaften_ddr" ~ "DDR-Meister",
    "davon_oberliga" ~ "Vor Bundesliga (inkl. BRD bis 1963)"
  ),
  ligasystem = factor(
    ligasystem, 
    levels = c("DDR-Meister", "Vor Bundesliga (inkl. BRD bis 1963)", "Bundesliga"))
  ) %>% 
  count(liga, ligasystem, wt = n_titel, name = "n_titel") %>% 
  filter(n_titel > 0) %>% 
  group_by(liga) %>% 
  arrange(desc(ligasystem), .by_group = TRUE) %>% 
  mutate(label_y_pos = cumsum(n_titel) - 0.5 * n_titel) %>% 
  ungroup() %>% #View()
  ggplot(aes(liga, n_titel, fill = ligasystem)) +
  geom_col(col = "white", linewidth = 0.2) +
  geom_label(
    aes(label = n_titel, y = label_y_pos),
    family = "Chivo", fill = alpha("white", 0.3), label.size = 0
  ) +
  scale_fill_manual(values = c("#FDB833", "#89B6A5", "#296EB4")) +
  labs(
    title = "Den Großteil ihrer Meistertitel gewannen die Zweitligisten
    vor Einführung der Bundesliga",
    subtitle = "Summe der Titel",
    caption = "Quelle: Wikipedia. Visualisierung: Ansgar Wolsing",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )
ggsave(here(base_path, "anzahl_meister-ligasystem-barchart.png"), width = 6, height = 4)


df %>% 
  filter(jemals_meister) %>% 
  mutate(davon_oberliga = meisterschaften - davon_meisterschaften_ddr - davon_bundesliga) %>% 
  pivot_longer(cols = c(davon_oberliga, davon_bundesliga, davon_meisterschaften_ddr),
               names_to = "ligasystem", values_to = "n_titel") %>% 
  mutate(ligasystem = case_match(
    ligasystem,
    "davon_bundesliga" ~ "Bundesliga",
    "davon_meisterschaften_ddr" ~ "DDR-Meister",
    "davon_oberliga" ~ "Vor Bundesliga (inkl. BRD bis 1963)"
  ),
  ligasystem = factor(
    ligasystem, 
    levels = c("DDR-Meister", "Vor Bundesliga (inkl. BRD bis 1963)", "Bundesliga"))
  ) %>% 
  count(liga, team, ligasystem, wt = n_titel, name = "n_titel") %>% 
  mutate(team = tidytext::reorder_within(team, by = n_titel, within = liga)) %>% 
  filter(n_titel > 0) %>% 
  ggplot(aes(team, n_titel, fill = ligasystem)) +
  geom_col(col = "white", linewidth = 0.2) +
  tidytext::scale_x_reordered() +
  scale_fill_manual(values = c("#FDB833", "#89B6A5", "#296EB4")) +
  coord_flip() +
  facet_wrap(vars(liga), scales = "free_y") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Den Großteil ihrer Meistertitel gewannen die Zweitligisten vor
    Einführung der Bundesliga",
    subtitle = "Summe der Titel",
    caption = "Quelle: Wikipedia. Visualisierung: Ansgar Wolsing",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )



# /* CSS HEX */
#   --cambridge-blue: #89b6a5ff;
#   --jasmine: #ffd07bff;
#   --xanthous: #fdb833ff;
#   --azul: #296eb4ff;
#   --bleu-de-france: #1789fcff;
