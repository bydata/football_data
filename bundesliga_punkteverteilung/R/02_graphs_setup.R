library(tidyverse)

## visualize timeline -----------------------------------------

library(tidyverse)

windowsFonts(Georgia = windowsFont("Georgia"))
windowsFonts(CenturyGothic = windowsFont("CenturyGothic"))

custom_arrow <- arrow(type = "closed", angle = 15, length = unit(0.125, "inches"))

# custom ggplot2 theme based on theme_minimal
custom_theme <- theme_minimal() + theme(
  text = element_text(color = "grey35", size = 9, family = "CenturyGothic"),
  plot.title = element_text(size = 14, margin = margin(t = 15, b = 10), family = "Georgia"),
  plot.subtitle = element_text(size = 11, margin = margin(b = 12)),
  plot.caption = element_text(size = 8, margin = margin(t = 8)),
  strip.text = element_text(size = 9, margin = margin(t = 10, b = 4, l = 4, r = 4)),
  axis.text = element_text(family = "CenturyGothic"),
  panel.grid = element_line(color = "grey98", size = 0.5),
  panel.grid.minor.y = element_blank(),
  axis.title.x = element_text(hjust = 0),
  axis.title.y = element_text(hjust = 1)
)

base_color <- "grey60"
base_text_color <- "grey25"
highlight_color <- "orangered"

# update fill/color for geoms
update_geom_defaults("line", list("color" = base_color))
update_geom_defaults("col", list("fill" = base_color))
update_geom_defaults("bar", list("fill" = base_color))
update_geom_defaults("point", list("color" = base_color))
update_geom_defaults("segment", list("color" = base_color))

# discrete color palettes
colors_discrete_5 <- scales::seq_gradient_pal(rgb(128,166, 206, maxColorValue = 255), "orangered", "Lab")(seq(0, 1, length.out = 5))
colors_discrete_8 <- scales::seq_gradient_pal(rgb(128,166, 206, maxColorValue = 255), "orangered", "Lab")(seq(0, 1, length.out = 8))
