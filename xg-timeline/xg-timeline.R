library(tidyverse)
library(ggtext)
library(worldfootballR)

base_path <- "xg-timeline"

# Get the shot logs for a given match (URL collected on understat.com)
match_url <- "https://understat.com/match/10393"
shots <- understat_match_shots(match_url)
glimpse(shots)

# Match result
match_date <- format(as_date(shots$date[1]), "%b %d, %Y")
home_team <- shots$home_team[1]
away_team <- shots$away_team[1]
home_goals <- shots$home_goals[1]
away_goals <- shots$away_goals[1]
match_title <- sprintf(
  "%s vs %s<br><span style='font-size:20pt'>%d - %d</span>",
  home_team, away_team, home_goals, away_goals)

# Select relevant variables and recode variables
xG_timeline <- shots %>% 
  mutate(
    team = ifelse(home_away == "h", home_team, away_team),
    team = factor(team, levels = c(home_team[1], away_team[1])),
    id = as.numeric(id)) %>% 
  select(id, minute, team, player, xG, result)

# Add match events for the kick-off and the last event
# Determine start and end minutes to expand the lines
start_minute <- 0
end_minute <- pmax(max(shots$minute), 90)
xG_timeline <- xG_timeline %>% 
  bind_rows(
    data.frame(
      id = c(rep(-Inf, 2), rep(Inf, 2)),
      minute = c(rep(0, 2), rep(end_minute, 2)),
      team = factor(c(rep(unique(xG_timeline$team), 2)),
                    levels = c(home_team[1], away_team[1])),
      player = NA, xG = 0)) %>% 
  group_by(team) %>% 
  arrange(id, .by_group = TRUE) %>% 
  mutate(xG_cumul = cumsum(xG)) %>% 
  ungroup()

bg_color <- "#3E3E40"
base_plot <- xG_timeline %>% 
  ggplot(aes(minute, xG_cumul, col = team, group = team)) +
  scale_x_continuous(
    breaks = seq(0, 90, 15),
    expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_color_manual(
    values = c("#FDE100", "#BA3733"), aesthetics = c("color", "fill")
  ) +
  coord_cartesian(clip = "off") +
  guides(color = "none") +
  labs(
    title = match_title,
    subtitle = match_date,
    caption = "Source: Understat.com",
    x = NULL, y = "xG", fill = NULL
  ) +
  theme_minimal(base_family = "Barlow", base_size = 10) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.background = element_rect(color = "grey20", fill = NA, linewidth = 0.3),
    text = element_text(color = "#CDD1D0"),
    axis.text = element_text(color = "#CDD1D0"),
    plot.title = element_markdown(
      family = "Barlow SemiBold", hjust = 0.5, lineheight = 1.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_line(color = "grey20"),
    panel.grid.major = element_line(size = 0.3),
    panel.grid.minor = element_line(size = 0.1),
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(color = NA, fill = alpha(bg_color, 0.2))
  )

base_plot + 
  geom_step(aes(color = team), size = 1) +
  geom_point(
    data = ~subset(., result == "Goal"),
    aes(fill = team),
    size = 2.5, shape = 21, color = bg_color, stroke = 1
  )
ggsave(file.path(base_path, "xG-timeline.png"), width = 5, height = 4)


# Version with shaded area -----------------------------------------------------

# first and last row in data frame by team
row_boundaries_by_team <- xG_timeline %>% 
  mutate(row = row_number()) %>% 
  group_by(team) %>% 
  summarize(
    first_row = min(row),
    last_row = max(row))

# Expand the data frame to get "steps" connecting each event by team
xG_timeline_fill <- data.frame(
  minute = rep(xG_timeline$minute, 2)[-row_boundaries_by_team$first_row],
  team = rep(xG_timeline$team, 2)[-row_boundaries_by_team$first_row],
  xG_cumul = rep(xG_timeline$xG_cumul, 2)[-row_boundaries_by_team$last_row]
)


base_plot + 
  geom_area(
    data = xG_timeline_fill,
    aes(fill = stage(team, after_scale = alpha(color, 0.2))),
    # change to position_dodge so that the areas are not stacked on top of each other
    position = position_dodge(width = 0),
    size = 1, stat = "align") +
  geom_point(
    data = ~subset(., result == "Goal"),
    aes(fill = team),
    size = 2.5, shape = 21, color = bg_color, stroke = 1
  )
ggsave(file.path(base_path, "xG-timeline-area.png"), width = 5, height = 4)
