## Recreate Tiermaker football clubs chart https://twitter.com/fussballtwit/status/1308145869984796672



library(tidyverse)
library(ggtext)
library(showtext)


# fonts
font_add("Open Sans", "OpenSans-Regular.ttf")
font_add("Open Sans Light", "OpenSans-Light.ttf")
font_add("Source Serif Pro SemiBold", "SourceSerifPro-SemiBold.ttf")
showtext_auto()

# place club icons in folder 
icon_path <- "input/icons"
icon_files <- list.files(icon_path, pattern = ".png")

labels <- c("The club of<br>your heart",
            "Big<br>like",
            "Small<br>like",
            "Totally<br>neutral",
            "Meh",
            "Small<br>dislike",
            "Big<br>dislike",
            "Absolutely<br>disgusting"
            )

colors <- c("#FF7F7E", "#FCC17D", "#FBE182", "#FEFD81", "#BEFE80", "#7EFF80", "#7FFFFF", "#7BBFFF")

df <- tibble(
  id = seq_along(labels),
  label = fct_inorder(labels)
)

# place all clubs in this tibble (icon = file name)
df_icons <- tribble(
  ~id, ~icon,
  1, "bvb",
  2, "fcliverpool",
  3, "fckoeln",
  3, "msv",
  4, "augsburg",
  5, "hsv",
  6, "psg",
  7, "fcbayern",
  7, "schalke",
  8, "rb",
) %>% 
  mutate(icon_html = glue::glue("<img src='{icon_path}/{icon}.png' width='25' style='background-color:transparent'>")) %>% 
  group_by(id) %>% 
  mutate(rank = rank(id, ties.method = "first")) %>% 
  ungroup()
  

left_join(df, df_icons, by = "id") %>% 
ggplot(aes(reorder(label, -id))) +
  geom_col(aes(y = 4), 
           fill = "white", alpha = 1,
           width = 0.99) +
  geom_col(aes(fill = label, y = 1),
           width = 0.99,
           show.legend = FALSE, position = "fill") +
  geom_richtext(aes(label = label, y = 0.025),
                label.color = NA, fill = NA,
            family = "Open Sans Light",
            size = 6, lineheight = 0.5,
            hjust = 0) +
  geom_richtext(aes(label = icon_html, y = rank),
            label.color = NA, fill = NA,
            hjust = 0) +
  scale_fill_manual(values = colors) +
  coord_flip(ylim = c(0, 4), expand = FALSE) +
  theme_void(base_family = "Open Sans Light") +
  theme(plot.background = element_rect(color = NA, fill = "grey20"))

ggsave("plots/football_club_favorites.png", type = "cairo", dpi = 320, width = 2, height = 3)
