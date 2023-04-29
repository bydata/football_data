library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)
library(lubridate)
library(ggbump)

base_path <- "haaland"

season_end_years <- 1993:2023
player_league_stats <- map(
  season_end_years,
  ~fb_league_stats(
    country = "ENG",
    gender = "M",
    season_end_year = .x,
    tier = "1st",
    stat_type = "shooting",
    team_or_player = "player"
  )
)

# wrapper to extract top goalscorer(s) for a given season
select_top_goalscorer <- function(df) {
  df %>% 
    slice_max(order_by = Gls, n = 1, with_ties = TRUE) %>% 
    select(Player, Nation, Pos, Squad, Born, Gls, PK, PKatt)
}

top_goalscorers <- map(player_league_stats, select_top_goalscorer) %>% 
  set_names(season_end_years) %>% 
  bind_rows(.id = "season_end_year")
write_rds(top_goalscorers, here(base_path, "top_goalscorers.rds"))

top_goalscorers <- read_rds(here(base_path, "top_goalscorers.rds"))

# get player URLs
league_urls <- map_chr(
  season_end_years,
  ~fb_league_urls(
    country = "ENG", 
    gender = "M", 
    season_end_year = .x,
    tier = "1st"
    )
  )

team_urls <- map(
  league_urls,
  fb_teams_urls
)
flatten(team_urls) %>% as.character()


top_goalscorers_names <- unique(top_goalscorers$Player) 
top_goalscorers_names

# 
top_goalscorers_urls <- 
  c(
    "https://fbref.com/en/players/4e56eef6/Teddy-Sheringham",
    "https://fbref.com/en/players/b6638a2c/Andy-Cole",
    "https://fbref.com/en/players/438b3a51/Alan-Shearer",
    "https://fbref.com/en/players/a0886d18/Dion-Dublin",
    "https://fbref.com/en/players/88b3f52c/Michael-Owen",
    "https://fbref.com/en/players/83c38e36/Chris-Sutton",
    "https://fbref.com/en/players/db8a04d1/Jimmy-Floyd-Hasselbaink",
    "https://fbref.com/en/players/2d0a99d3/Dwight-Yorke",
    "https://fbref.com/en/players/23d97ca1/Kevin-Phillips",
    "https://fbref.com/en/players/c0c5ee74/Thierry-Henry",
    "https://fbref.com/en/players/80a30989/Ruud-van-Nistelrooy",
    "https://fbref.com/en/players/945dea33/Didier-Drogba",
    "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo",
    "https://fbref.com/en/players/931ed5e9/Nicolas-Anelka",
    "https://fbref.com/en/players/f5781978/Dimitar-Berbatov",
    "https://fbref.com/en/players/c1a26d43/Carlos-Tevez",
    "https://fbref.com/en/players/3fcab3a8/Robin-van-Persie",
    "https://fbref.com/en/players/a6154613/Luis-Suarez",
    "https://fbref.com/en/players/4d034881/Sergio-Aguero",
    "https://fbref.com/en/players/21a66f6a/Harry-Kane",
    "https://fbref.com/en/players/e342ad68/Mohamed-Salah",
    "https://fbref.com/en/players/d5dd5f1f/Pierre-Emerick-Aubameyang",
    "https://fbref.com/en/players/c691bfe2/Sadio-Mane",
    "https://fbref.com/en/players/45963054/Jamie-Vardy",
    "https://fbref.com/en/players/92e7e919/Son-Heung-min",
    "https://fbref.com/en/players/1f44ac21/Erling-Haaland"
  )

top_goalscorers_seasons_urls <- top_goalscorers %>% 
  select(Player, season_end_year) %>% 
  inner_join(
    data.frame(
      Player = top_goalscorers_names,
      Player_url = top_goalscorers_urls
    ), 
    by = "Player"
  )

top_goalscorers_match_logs <- map2(
  top_goalscorers_seasons_urls$Player_url, 
  top_goalscorers_seasons_urls$season_end_year, 
  fb_player_match_logs,
  stat_type = "summary"
  )

write_rds(top_goalscorers_match_logs, here(base_path, "top_goalscorers_match_logs-20230217.rds"))

top_goalscorers_match_logs <- read_rds(here(base_path, "top_goalscorers_match_logs-20230217.rds"))

# Haaland
haaland_match_logs <- fb_player_match_logs(
  "https://fbref.com/en/players/1f44ac21/Erling-Haaland", 
  season_end_year = 2023,
  stat_type = "summary")

top_goalscorers_match_logs[[length(top_goalscorers_match_logs)]] <- haaland_match_logs

#' How many top goalscorers had less goals at the end of the season
#' than Haaland already has at this point of the season?
# goals_haaland <- top_goalscorers$Gls[top_goalscorers$season_end_year == 2023]
goals_haaland <- top_goalscorers_match_logs[[length(top_goalscorers_match_logs)]] %>% 
  filter(Comp == "Premier League") %>% 
  summarize(sum(Gls_Performance)) %>% 
  pull()

top_goalscorers %>% 
  filter(season_end_year < 2023) %>% 
  filter(Gls < goals_haaland) %>% 
  distinct(season_end_year)
top_goalscorers %>% 
  filter(season_end_year < 2023) %>% 
  filter(Gls >= goals_haaland) %>% 
  distinct(season_end_year)

top_goalscorers %>% 
  filter(season_end_year < 2023) %>% 
  filter(Gls > goals_haaland) %>% 
  distinct(Player, season_end_year)

  
relevant_players <- c("Erling Haaland", "Andy Cole", "Alan Shearer",
                      "Mohamed Salah", "Cristiano Ronaldo", "Luis Suárez")

# save the current matchweek for the current season
matchweeks_max_current_season <- top_goalscorers_match_logs[[length(top_goalscorers_match_logs)]] %>% 
  filter(Comp == "Premier League") %>% 
  mutate(matchweek = as.numeric(str_extract(Round, "\\d+"))) %>% 
  summarize(matchweek = max(matchweek)) %>% 
  pull(matchweek)

p <- 
  top_goalscorers_match_logs %>% 
  bind_rows() %>% 
  filter(Comp == "Premier League") %>% 
  mutate(matchweek = as.numeric(str_extract(Round, "\\d+"))) %>% 
  arrange(Player, Date, matchweek) %>% 
  count(Player, Season, matchweek, wt = Gls_Performance, name = "gls") %>% 
  group_by(Player, Season) %>% 
  complete(matchweek = 1:42, fill = list(gls = 0)) %>% 
  mutate(gls_cumul = cumsum(gls)) %>% 
  ungroup() %>% 
  # 42 matchweeks only to season 1994-1995, after that 38 
  filter(!(Season > "1994-1995" & matchweek > 38) & 
           !(Season == max(Season) & matchweek > matchweeks_max_current_season)) %>%
  # TEMP
  # add_row(Player = "Erling Haaland", Season = "2022-2023", matchweek = 30, gls = 2, gls_cumul = 30) %>% 
  ggplot(aes(matchweek, gls_cumul, group = paste(Player, Season, sep = "#"))) +
  geom_bump(
    smooth = 5, color = "grey50", linewidth = 0.1
  ) +
  geom_bump(
    data = ~subset(., Player %in% relevant_players) %>%
      mutate(Player2 = factor(Player, levels = relevant_players)),
    aes(col = Player2 == "Erling Haaland"),
    smooth = 5, linewidth = 0.7, show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(10, 50, 10)) +
  scale_color_manual(values = c("TRUE" = "#FF9B42", "FALSE" = "#00A7E1")) +
  facet_wrap(vars(Player2)) +
  labs(
    title = "<span style='color:#FF9B42'>Erling Haaland</span> keeps on scoring",
    # ... already surpassing the final tally of the top scorers of 21 seasons
    subtitle = glue::glue(
    "Haaland has scored {goals_haaland} Premier League goals after
    {matchweeks_max_current_season} matchweeks. 
    The graph shows Haaland's goalscoring progression compared to the other 
    season's top scorers in the Premier League. 
    The top 5 season top goalscorers are presented. The progress of all other topscorers
    is shown in the background.
    "),
    caption = "Data: FBRef, {worldfootballR} package. Visualisation: Ansgar Wolsing",
    x = "Matchweek",
    y = "Goals scored"
  ) +
  theme_minimal(base_family = "Outfit", base_size = 10) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey89", linewidth = 0.2),
    panel.grid.minor.x = element_line(color = "grey89", linewidth = 0.05),
    panel.grid.major.y = element_line(color = "grey89", linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "grey89", linewidth = 0.05),
    text = element_text(lineheight = 1.1, color = "grey32"),
    axis.text = element_text(lineheight = 1.1, color = "grey40"),
    plot.title = element_markdown(family = "Outfit SemiBold", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.92),
    plot.caption = element_markdown()
  )
ggsave(here(base_path, "haaland-pl-goalscorers-20230429.png"), width = 8, height = 5)


top_goalscorers_match_logs %>% 
  bind_rows() %>% 
  filter(Comp == "Premier League") %>% 
  count(Player, Season, Round, wt = Gls_Performance, name = "gls") %>% 
  group_by(Player, Season) %>% 
  mutate(gls_cumul = cumsum(gls)) %>% 
  ungroup() %>% 
  filter(Round == "Matchweek 27") %>% 
  select(Player, Season, gls_cumul) 
