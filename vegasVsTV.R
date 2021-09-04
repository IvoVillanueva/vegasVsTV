
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rvest)
library(espnscrapeR)
library(lubridate)
library(janitor)
library(ggimage)
library(ggtext)


# Custom theme_ivo --------------------------------------------------------


theme_ivo <- function() {
  theme_minimal(base_size = 9, base_family = "Chivo") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f4f4f4", color = "#f4f4f4"),
      plot.caption = element_markdown(size = 5, hjust = .5)
    )
}

# Data Wrangler -----------------------------------------------------------

e <- get_nfl_schedule(2021)

night <- e %>%
  filter(season == 2021, broadcast_name %in% c("NBC", "ESPN"), type == 2) %>%
  select(
    home_team_full,
    away_team_full
  ) %>%
  add_row(
    home_team_full = c("Green Bay Packers", "Pittsburgh Steelers"),
    away_team_full = c("Minnesota Vikings", "Cleveland Browns")
  ) %>% # rows manuales porque el calendario solo está hasta el 2021 y estos partidos se juegan en 2022
  pivot_longer(cols = home_team_full:away_team_full) %>%
  group_by(value) %>%
  count(value) %>%
  rename(tm = value, games = n)


url <- "https://www.vegasinsider.com/nfl/odds/win-totals/"
vegas <- url %>%
  read_html() %>%
  html_element("table:nth-of-type(2) table") %>%
  html_table() %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  rename(tm = "team") %>%
  mutate(
    tm = case_when(
      tm == "Washington Football Team" ~ "Washington",
      TRUE ~ tm
    ),
    win_total = as.numeric(win_total)
  )

logos <- get_nfl_teams() %>%
  select(tm = team_full_name, logo)

df <- left_join(vegas, night) %>% left_join(logos)

# plot --------------------------------------------------------------------


p <- df %>%
  mutate(games = replace_na(games, 0)) %>%
  ggplot(aes(x = games, y = win_total)) +
  geom_image(
    aes(
      image = logo
    ),
    position = position_jitter(width = .25, height = .35), size = .065, by = "width"
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(2.5, 13), breaks = seq(3, 13, 1)) +
  scale_x_continuous(limits = c(-.5, 4.5), breaks = seq(0, 4, 1)) +
  theme_ivo() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 15, 10)
  ) +
  labs(
    x = "Sunday & Monday Night TV Games",
    y = "NFL Win Totals Betting Odds",
    title = "Do The Best Teams Play On Sunday & Monday Night Games The Most?",
    subtitle = "Vegas win totals and Sunday & Monday Night Games for the 2021-22 NFL Season",
    caption = "<br>**Datos**: *espnscrapR & vegasinsider.com*  **Gráfico**: *Ivo Villanueva @elcheff*<br>"
  )

ggsave("games.png", p, w = 6, h = 6, dpi = 300)

# 4 de Septiembre 2021 -----------------------------------------------------
# Ivo Villanueva ----------------------------------------------------------
