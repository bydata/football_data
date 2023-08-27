library(tidyverse)
library(ggtext)
library(here)
library(treemapify)
library(worldfootballR)
library(grid)

base_path <- "bocbvb-marktwerte-2023"

# Retrieve Transfermarkt market value estimates
market_values <- tm_player_market_values("Germany", start_year = 2023)
write_rds(market_values, here(base_path, "bundesliga-market-values-2023.rds"))

market_values %>% 
  group_by(squad) %>% 
  summarize(total_value = sum(player_market_value_euro, na.rm = TRUE)) %>% 
  arrange(-total_value)


market_values %>% 
  mutate(player_market_value_euro = replace_na(player_market_value_euro, 0)) %>% 
  group_by(squad) %>% 
  mutate(player_team_market_value_rank = rank(
    -player_market_value_euro, ties.method = "first")) %>% 
  ungroup() %>% 
  arrange(squad, player_team_market_value_rank) %>% 
  filter(squad %in% c("VfL Bochum", "Borussia Dortmund")) %>% 
  View()  
  

ragg::agg_png(here(base_path, "marktwerte-bocbvb-2023.png"), 
              width = 8, height = 6, units = "in", res = 300)
market_values %>% 
  filter(squad %in% c("VfL Bochum", "Borussia Dortmund")) %>% 
  mutate(squad = factor(squad, levels = c("VfL Bochum", "Borussia Dortmund"))) %>% 
  arrange(squad, desc(player_name)) %>% 
  ggplot(aes(area = player_market_value_euro, subgroup = squad,
             subgroup2 = player_name,
             fill = squad, col = squad)) +
  geom_treemap(
    size = 1, radius = unit(1, "mm"), start = "bottomright") +
  geom_treemap_subgroup_text(family = "Outfit SemiBold", grow = FALSE,
                             start = "bottomright") +
  geom_treemap_subgroup2_text(
    family = "Outfit", place = "top", alpha = 0.8, size = 9, grow = FALSE, 
    reflow = TRUE, start = "bottomright") +
  geom_treemap_subgroup_border(col = "white", size = 6, start = "bottomright") +
  scale_fill_manual(values = c("#005CA9", "black")) +
  scale_color_manual(values = c("white", "#FDE100")) +
  guides(fill = "none", color = "none") +
  labs(
    title = glue::glue("Marktwerte <span style='color:#005CA9'>VfL Bochum</span>
                       vs. <span style='color:black'>Borussia Dortmund</span>"),
    caption = "**Daten:** Transfermarkt.de (Stand 26.08.2023). **Visualisierung:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "top",
    text = element_text(color = "grey28"),
    plot.title = element_markdown(
      family = "Outfit SemiBold", hjust = 0.5, size = 16, lineheight = 1.2, 
      angle = 0, margin = margin(t = 8, b = 4)),
    plot.caption = element_markdown(size = 7, lineheight = 1.2)
  )
# underline BVB in title
grid.lines(x = c(0.555, 0.8), y = c(0.935, 0.935),
           gp = gpar(col = "#FDE100", lwd = 4))
dev.off()

ragg::agg_png(here(base_path, "marktwerte-bocbvb-2023-equal-values.png"), 
              width = 8, height = 6, units = "in", res = 300)
market_values %>% 
  filter(squad == "VfL Bochum" | (squad == "Borussia Dortmund" & 
                                    player_name %in% c("Nico Schlotterbeck", "Felix Nmecha"))) %>% 
  mutate(squad = factor(squad, levels = c("VfL Bochum", "Borussia Dortmund"))) %>% 
  arrange(squad, player_name) %>% 
  ggplot(aes(area = player_market_value_euro, subgroup = squad,
             subgroup2 = player_name,
             fill = squad, col = squad)) +
  geom_treemap(size = 1, radius = unit(1, "mm")) +
  geom_treemap_subgroup_text(family = "Outfit SemiBold", grow = FALSE) +
  geom_treemap_subgroup2_text(
    family = "Outfit Light", place = "top", alpha = 0.8, size = 9, grow = FALSE, 
    reflow = TRUE) +
  geom_treemap_subgroup_border(col = "white", size = 6) +
  scale_fill_manual(values = c("#005CA9", "black")) +
  scale_color_manual(values = c("white", "#FDE100")) +
  guides(fill = "none", color = "none") +
  labs(
    title = glue::glue("Marktwerte <span style='color:#005CA9'>VfL Bochum</span>
                       vs. <span style='color:black'>Borussia Dortmund</span>"),
    caption = "**Daten:** Transfermarkt.de (Stand 26.08.2023). **Visualisierung:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "top",
    text = element_text(color = "grey28"),
    plot.title = element_markdown(
      family = "Outfit SemiBold", hjust = 0.5, size = 16, lineheight = 1.2, 
      angle = 0, margin = margin(t = 8, b = 0)),
    plot.caption = element_markdown(size = 7, lineheight = 1.2)
  )
# underline BVB in title
grid.lines(x = c(0.555, 0.8), y = c(0.935, 0.935),
           gp = gpar(col = "#FDE100", lwd = 4))
dev.off()
