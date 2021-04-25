library(here)
library(arrow)
library(tidyverse)
library(skimr)

setwd(here())

#pull in raw data
rosters <- read_parquet("data/rosters/rosters_1999_2019.pdata")
pbp <- read_parquet("data/pbp_data/pbp_reg_post_1999_2019.pdata")

#Pull in passer, rusher, and receiver positions
pbp <- pbp %>%
  left_join(dplyr::select(rosters,
                          rusher_gsis_id = GSIS_ID,
                          rusher_gsis_name = Player,
                          rusher_gsis_pos = Position),
            by = c("rusher_player_id" = "rusher_gsis_id")) %>%
  left_join(dplyr::select(rosters,
                          receiver_gsis_id = GSIS_ID,
                          receiver_gsis_name = Player,
                          receiver_gsis_pos = Position),
            by = c("receiver_player_id" = "receiver_gsis_id")) %>%
  left_join(dplyr::select(rosters,
                          passer_gsis_id = GSIS_ID,
                          passer_gsis_name = Player,
                          passer_gsis_pos = Position),
            by = c("passer_player_id" = "passer_gsis_id"))

#skim
skim(rosters)
skim(pbp)

#filter out bad data
explore <- pbp %>%
  filter(!is.na(posteam), #not real plays
         !is.na(alt_game_id)) #Pro Bowls

#skim again
skim(explore)

pass <- explore %>%
  filter(play_type == "pass") %>%
  group_by(season_year, season_type) %>%
  summarise(count = n(),
            comp_pct = mean(complete_pass, na.rm = TRUE),
            adot = mean(air_yards, na.rm = TRUE))

library(RColorBrewer)
library(scales)
nb.cols <- 14
mycolors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(nb.cols)

show_col(mycolors)

#completion rate over time
explore %>%
  filter(season_year >= 2006 & receiver_gsis_pos %in% c("RB")) %>%
  ggplot(aes(x=air_yards, y=complete_pass)) +
  geom_smooth(aes(color = as.factor(season_year)), se = F) +
  scale_colour_manual(values = mycolors) +
  geom_smooth(color = "black", se = F) +
  xlim(-5,30) +
  theme_bw() +
  facet_wrap(~receiver_gsis_pos)

explore %>%
  filter(season_year >= 2006 & receiver_gsis_pos %in% c("TE")) %>%
  ggplot(aes(x=air_yards, y=yards_gained)) +
  geom_smooth(aes(color = as.factor(season_year)), se = F) +
  scale_colour_manual(values = mycolors) +
  geom_smooth(color = "black", se = F) +
  xlim(-5,30) +
  theme_bw() +
  facet_wrap(~receiver_gsis_pos)

#completion rate over time
explore %>%
  filter(rusher_gsis_pos %in% c("RB") & play_type == "run") %>%
  ggplot(aes(x=yardline_100, y=rush_touchdown)) +
  geom_smooth(aes(color = as.factor(season_year)), se = F) +
  scale_colour_manual(values = mycolors) +
  geom_smooth(color = "black", se = F) +
  xlim(0,30) +
  theme_bw() +
  facet_wrap(~rusher_gsis_pos)