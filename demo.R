library(nflfastR)
library(tidyverse)
library(gt)
library(ggimage)

#load play-by-play data
pbp <- load_pbp(2020:2021)
pbp %>% head
pbp %>% select(posteam, defteam, season, down, ydstogo, rush, pass, yards_gained) %>% head

#if there is no down, probably it is special teams play
#therefore subset the dataset to exclude special teams
pbp_rp <- pbp %>%
  filter(rush == 1 | pass ==1) %>%
  filter(!is.na(epa))
 
pbp_rp %>%
  filter(posteam == "PIT") %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarize(passes = n(),
      avg_epa = mean(epa)) %>%
  arrange(-avg_epa)

offense2020 <- pbp_rp %>%
  filter(season == 2020) %>%
  group_by(posteam) %>%
  summarize(epa20 = mean(epa))

offense2021 <- pbp_rp %>%
  filter(season == 2021) %>%
  group_by(posteam) %>%
  summarize(epa21 = mean(epa))

offenses_all <- offense2020 %>%
  left_join(offense2021, by = "posteam")

offenses_all <- offenses_all %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

offenses_all %>%
  ggplot(aes(epa20, epa21)) +
  geom_hline(yintercept = mean(offenses_all$epa21)) +
  geom_vline(xintercept = mean(offenses_all$epa20)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_image(aes(team_logo_espn), size = 0.05, asp = 16/9) +
  theme_minimal()

