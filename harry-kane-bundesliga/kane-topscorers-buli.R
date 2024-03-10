library(tidyverse)
library(ggtext)
library(rvest)

base_path <- "harry-kane-bundesliga"

# Scrape goalscoring season (only Bundesliga)
scrape_goalscoring_season <- function(url) {
  selector <- "table.kick__table"
  page <- read_html(url)
  table <- page |> 
    html_node(css = selector) |> 
    html_table()
  
  df <- table |> 
    select(matchday = 2, goals = Tore) |> 
    # drop the first 2 rows
    slice_tail(n = nrow(table) - 2) |> 
    # remove other competitions
    filter(str_detect(matchday, "^\\d{1,2}\\.\\sSpieltag")) |> 
    mutate(
      matchday = str_extract(matchday, "\\d+"),
      matchday = as.numeric(matchday),
      goals = ifelse(goals == "-", "0", goals),
      goals = as.numeric(goals)
    )
  df
}

urls <- c(
  "Müller 1971/72" = "https://www.kicker.de/gerd-mueller/spieler-einsaetze/bundesliga/1971-72/fc-bayern-muenchen",
  "Lewandowski 2021/22" = "https://www.kicker.de/robert-lewandowski/spieler-einsaetze/bundesliga/2020-21/fc-bayern-muenchen",
  "Kane 2023/24" = "https://www.kicker.de/harry-kane/spieler-einsaetze/bundesliga/2023-24/fc-bayern-muenchen"
)

goalscoring_seasons <- map(urls, scrape_goalscoring_season)

df_plot <- goalscoring_seasons |> 
  bind_rows(.id = "player_season") |> 
  arrange(player_season, matchday) |> 
  mutate(
    goals_cumul = cumsum(goals),
    .by = player_season
  )

df_plot |> 
  mutate(player_season = fct_rev(player_season)) |> 
  ggplot(aes(matchday, goals_cumul, color = player_season)) +
  geom_step(
    aes(linewidth = ifelse(player_season == "Kane 2023/24", 0.8, 0.3))) +
  geom_point(
    data = group_by(df_plot, player_season) |> 
      filter(matchday == max(matchday)) |> 
      ungroup(),
    size = 2
  ) +
  geom_text(
    data = data.frame(
      player_season = unique(df_plot$player_season), 
      x = c(26, 26, 21), 
      y = c(30, 36, 20),
      hjust = c(0, 1, 0)),
    aes(x + .25, y, label = player_season, hjust = hjust),
    family = "Chivo", size = 2.5
  ) + 
  scale_color_manual(values = c("grey60", "#222222", "#DD2222")) +
  scale_linewidth_identity() +
  coord_cartesian(clip = "off") +
  guides(color = "none") +
  labs(
    title = "<span style='color:#DD2222'>Harry Kane</span> on track with the
    historic Bundesliga goalscoring record",
    caption = "Source: kicker.de. Visualization: Ansgar Wolsing",
    x = "Matchday", y = "Cumulative goals"
  ) +
  theme_minimal(base_family = "Outfit Light") +
  theme(
    plot.background = element_rect(color = "#FCFCFC", fill = "#FCFCFC"),
    plot.title = element_markdown(family = "Outfit SemiBold"),
    plot.title.position = "plot"
  )
ggsave(file.path(base_path, "kane-goalscoring-2024.png"), width = 3, height = 2, 
       scale = 2)
