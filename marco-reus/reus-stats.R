library(tidyverse)
library(ggtext)
library(worldfootballR)
library(here)

base_path <- here("marco-reus")

reus_url <- "https://fbref.com/en/players/36a3ff67/Marco-Reus"
stats <- fb_player_season_stats(reus_url, stat_type = "standard")
unique(stats$Squad)

# Prepare data for the plot
goals_assists <- stats |> 
  mutate(
    season_end_year = str_extract(Season, "-(\\d{4})", group = 1),
    season_end_year = as.numeric(season_end_year),
    Squad = fct_reorder(Squad, season_end_year)) |> 
  group_by(season_end_year, Squad) |> 
  summarize(
    Gls = sum(Gls),
    Ast = sum(Ast),
    .groups = "drop") |> 
  pivot_longer(cols = c(Gls, Ast), names_to = "stat_type", values_to = "value")


# Titles and labels
plot_titles <- list(
  "en" = list(
    title = "Marco Reus Career Goals and Assists",
    subtitle = "**Goals** and <b style='color:grey70'>assists</b> in all 
    competitions for<br>
    <b style='color:#E20024'>Rot Weiss Ahlen</b>, 
    <b style='color:white'>Borussia M'Gladbach</b>, and
    <b style='color:#FDE100'>Borussia Dortmund</b>",
    caption = "2023-2024 figures as of matchday 32.<br>
    Source: FBRef.com.
    Image credit: Tim Reckmann (CC BY-SA 3.0). 
    Visualization: Ansgar Wolsing",
    x = NULL, y = "# of goals and assists",
    annotation_goals_assists = c("Goals", "Assists")
  ),
  "de" = list(
    title = "Marco Reus' Tore und Assists",
    subtitle = "**Tore** und <b style='color:grey70'>Torvorlagen</b> in allen 
    Wettbewerben für<br>
    <b style='color:#E20024'>Rot Weiss Ahlen</b>, 
    <b style='color:white'>Borussia M'Gladbach</b> und
    <b style='color:#FDE100'>Borussia Dortmund</b>",
    caption = "Saison 2023-24 bis 32. Spieltag<br>
    Daten: FBRef.com.
    Bild: Tim Reckmann (CC BY-SA 3.0). 
    Visualisierung: Ansgar Wolsing",
    x = NULL, y = "Anzahl Tore und Torvorlagen",
    annotation_goals_assists = c("Tore", "Assists")
  )
)

# Gradient fill for the plot background
gradient_fill <- grid::linearGradient(c("grey6", "grey14"))


# Creates the plot with labels in the given language defined in plot_titles
plot_with_labels <- function(lang) {
  
  stopifnot(lang %in% names(plot_titles))
  
  goals_assists |> 
    ggplot(aes(season_end_year, value, group = stat_type, fill = Squad)) +
    geom_col(aes(alpha = stat_type, col = Squad), width = 0.8, linewidth = 0.3) +
    # insert picture 
    annotate(
      GeomRichtext,
      x = 2024, y = 35,
      label = sprintf(
        "<img src='%s' width='40'>", 
        here(base_path, "reus-picture.png")),
      fill = NA, label.size = 0
    ) +
    # annotations for goals and assists
    annotate(
      "text",
      x = 2018,
      y = c(26, 32),
      label = plot_titles[[lang]]$annotation_goals_assists,
      color = "grey76", hjust = 1, family = "Avenir", size = 3 
    ) +
    annotate(
      GeomCurve,
      x = 2017.5, xend = 2018.5,
      y = c(25, 31), yend = c(18, 29), 
      color = "grey76", linewidth = 0.2, curvature = 0.3,
      arrow = arrow(angle = 20, length = unit(1, "mm"), type = "open")
    ) +
    scale_fill_manual(
      values = c("Rot Weiss Ahlen" = "#E20024", "M'Gladbach" = "white", 
                 "Dortmund" = "#FDE100"),
      aesthetics = c("fill", "color")) +
    scale_x_continuous(
      breaks = seq(min(goals_assists$season_end_year),
                   max(goals_assists$season_end_year), 2)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_alpha_manual(values = c(0.6, 0.9)) +
    coord_cartesian(clip = "off") +
    guides(fill = "none", color = "none", alpha = "none") +
    labs(
      title = plot_titles[[lang]]$title,
      subtitle = plot_titles[[lang]]$subtitle,
      caption = plot_titles[[lang]]$caption,
      x = plot_titles[[lang]]$x, y = plot_titles[[lang]]$y
    ) +
    theme_minimal(base_family = "Avenir", base_size = 10) +
    theme(
      plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
      text = element_text(color = "grey99"),
      axis.title = element_text(color = "grey76"),
      axis.text = element_text(color = "grey76"),
      plot.title = element_text(family = "Bangers", hjust = 0.5, size = 18),
      plot.subtitle = element_textbox(
        lineheight = 1.25, width = 1.1, hjust = 0.5, halign = 0.5,
        margin = margin(t = 6, b = 2)),
      plot.caption = element_markdown(
        hjust = 0, color = "grey76", size = 6, lineheight = 1.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey30", linewidth = 0.2),
      panel.grid.minor.y = element_line(color = "grey30", linewidth = 0.1),
      legend.position = "bottom"
    )
}

plot_with_labels("en")
ggsave(here(base_path, "reus-goals-assists-career-en.png"),
       width = 5, height = 5)  

plot_with_labels("de")
ggsave(here(base_path, "reus-goals-assists-career-de.png"),
       width = 5, height = 5)  
