library(tidyverse)
library(purrr)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2021-11-30')
matches  <- tuesdata$matches

glimpse(matches)

library(corrr)

matches |> 
  select(where(is.numeric)) |> 
  corrr::correlate()

library(ggcorrplot)
matches |> 
  group_by(margin_type) |> 
  summarize(count = n(), mean = mean(margin, na.rm = TRUE))

##### cleaning #####
glimpse(matches)


##### feature creation #####
match_dat <- 
  matches |> 
  mutate(match_id = str_extract(match_id, pattern = "(?<= # ).*"),
         team1_home_away = as_factor(team1_away_or_home),
         team2_home_away = as_factor(team2_home_away),
         margin_type = as_factor(margin_type),
         margin_type = forcats::fct_collapse(margin_type,
                                             runs = c("run", "runs"),
                                             wickets = c("wicket", "wickets")),
         toss_decision = as_factor(toss_decision),
         match_date = lubridate::as_date(match_date, format = "%b %d, %Y")) |> 
  select(-team1_away_or_home) |> glimpse()

##### exploring #####
match_dat |>
  drop_na(match_date) |> 
  select(match_date, starts_with('score')) |> 
  mutate(year = as_factor(lubridate::year(match_date))) |> 
  mutate(total_score = score_team1 + score_team2) |> 
  ggplot(aes(x = year, y = total_score)) +
  geom_violin(aes(fill = year), 
              alpha = .7, 
              scale = 'area',
              draw_quantiles = TRUE) +
  stat_summary(fun = mean, 
               geom = 'point', 
               shape = 23, 
               size = 2, 
               fill = "white") +
  guides(fill = 'none') +
  theme_bw() +
  labs(title = "Distribution of Total Scores by Year",
       x = "year", y = "total score per game")

match_dat |>
  select(match_date, 
         team1, team2, winner, 
         team1_home_away, team2_home_away, 
         score_team1, score_team2) |>
  mutate(which_team_won  = if_else(winner == team1, 
                                   "team1", 
                                   "team2"),
         winner_location = if_else(which_team_won == "team1", 
                                   team1_home_away,
                                   team2_home_away),
         winner_score = if_else(score_team1 > score_team2,
                                score_team1,
                                score_team2)) |> 
  # select(-c(winner, starts_with("team"), starts_with("score"), starts_with("which"))) |> 
  drop_na()
