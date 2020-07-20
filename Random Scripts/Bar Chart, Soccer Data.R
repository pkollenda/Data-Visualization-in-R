https://cedricscherer.netlify.app/2019/05/17/the-evolution-of-a-ggplot-ep.-1/#aim

library(readr)
library(tidyverse)
library(countrycode)
library(ggsci)
library(showtext)
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

df <- read_csv("Downloads/results.csv")
df <- df %>% mutate(home_continent = countrycode(home_team, origin = "country.name", destination = "continent"),
                    away_continent = countrycode(away_team, origin = "country.name", destination = "continent")) %>% 
  mutate(home_continent = case_when(
    home_team %in% c("England", "Scotland", "Wales", "Northern Ireland", "Czechoslovakia", "Yugoslavia", "German DR", "Catalonia", "Basque Country") ~ "Europe",
    home_team %in% c("Zanzibar") ~ "Africa",
    T ~ home_continent
  )) %>% 
  mutate(away_continent = case_when(
    away_team %in% c("England", "Scotland", "Wales", "Northern Ireland", "Czechoslovakia", "Yugoslavia", "German DR", "Catalonia", "Basque Country") ~ "Europe",
    away_team %in% c("Zanzibar") ~ "Africa",
    T ~ away_continent
  )) %>% filter(!is.na(away_continent) & !is.na(home_continent))
df <- df %>% mutate(home_points = if_else(home_score > away_score, 3, if_else(home_score == away_score, 1, 0)),
                    away_points = if_else(home_score < away_score, 3, if_else(home_score == away_score, 1, 0)))
home <- df %>% group_by(home_team) %>% summarise(points = sum(home_points), games = n(), continent = first(home_continent))
away <- df %>% group_by(away_team) %>% summarise(points = sum(away_points), games = n(), continent = first(away_continent))
df <- full_join(home, away, by = c("home_team" = "away_team")) %>% mutate(points = points.x + points.y, games = games.x + games.y) %>% 
  select(-c(points.x, points.y, games.x, games.y, continent.x)) %>% rename(continent = continent.y, country = home_team)
df <- df %>% mutate(across(everything(), replace_na, 0)) %>% mutate(points_per_game = if_else(games != 0, points / games, 0))
df <- df %>% filter(continent != "0")
df <- df %>% mutate(continent = fct_reorder(continent, points, mean))
df_contavg <- df %>% group_by(continent) %>% summarise(avgcontpoints = mean(points), avgcontgames = mean(games), avgcontppg = mean(points_per_game), .groups = "drop")
df <- full_join(df, df_contavg, by = "continent")
avg <- df %>% summarise(avgpoints = mean(points), avggames = mean(games), avgppg = mean(points_per_game))

# Plotting
theme_set(theme_light(base_size = 24, base_family = "Poppins"))

g <- ggplot(df, aes(x = continent, y = points, color = continent)) +
  coord_flip() + 
  scale_y_continuous(limits = c(0, 2500), expand = c(0.05, 0.05)) +
  scale_color_uchicago() +
  labs(y = "Number of Points", x = NULL) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 24),
    panel.grid = element_blank()
  )

g + 
  geom_segment(aes(x = continent, xend = continent, y = avg$avgpoints, yend = avgcontpoints), size = 1) +
  geom_hline(yintercept = avg$avgpoints, color = "gray70", size = 0.6) +
  geom_jitter(position = position_jitter(seed = 1896, width = 0.2), alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 5) 



