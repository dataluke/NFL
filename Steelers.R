library(nflfastR)
library(tidyverse)
library(gt)
library(ggimage)

pbp <- load_pbp()
pbp_rp <- pbp %>%
  filter(rush == 1 | pass ==1) %>%
  filter(!is.na(epa))

steelers_qb <- pbp_rp %>%
  filter(posteam == "PIT") %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarize(passes = n(),
            avg_epa = mean(epa)) %>%
  arrange(-avg_epa)

steelers_rb <- pbp_rp %>%
  filter(posteam == "PIT") %>%
  filter(!is.na(rusher_player_name)) %>%
  group_by(rusher_player_name) %>%
  summarize(rushes = n(),
            yards = sum(rushing_yards, na.rm = T),
            tds = sum(touchdown == 1 & td_team == posteam),
  ) %>%
  arrange(-yards) %>%
  head(10)
            
