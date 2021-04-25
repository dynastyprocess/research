library(ffscrapr)
library(tidyverse)
library(here)
library(rvest)
library(httr)


insert_mergename <- . %>%
  mutate(merge_name = name) %>%
  mutate_at('merge_name',str_remove_all,"( Jr)|( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('merge_name',str_squish) %>%
  mutate_at('merge_name',tolower)

dlf <- read_csv("C:\\Users\\syd23\\Documents\\DynastyProcess\\research\\misc_research\\dlf_values.csv") %>%
  insert_mergename() %>% 
  mutate(dlf_rank = row_number(-value))

#League IDs
# 22627 48
# 52290 AA
# 54040 SSB

#Connection
conn <- mfl_connect(2020, 54040, user_agent = "dynastyprocess", rate_limit_number = 30, rate_limit_seconds = 60)

mfl_roster <- ffscrapr::ff_rosters(conn)

clay_url <- read_html("https://www.espn.com/espn/print?id=15698900")

clay_table <- html_nodes(clay_url, "a") %>% html_text() %>% as_tibble() %>% slice(3:242) %>% 
  mutate(name = value,
         rank = row_number(),
         value = 1000*exp(rank*-0.02145)) %>% 
  insert_mergename()

joined <- dlf %>% 
  left_join(clay_table, by = "merge_name") %>% 
  mutate(value_gap = value.x-value.y)


#Scrape whole year
# mfl_points <- tibble()
# for (i in 1:16) {mfl_points <- mfl_points %>% rbind(ffscrapr::ff_playerscores(conn, season = 2020, week = i))}
#
# mfl_points_summ <- mfl_points %>%
#   group_by(player_id) %>%
#   summarise(avg_pts = mean(as.numeric(points), na.rm = TRUE),
#             games = n()) %>%
#   ungroup()

values <- ffscrapr::dp_values() %>%
  mutate(joe_value = 10500*exp(ecr_1qb*-0.0175),
         joe_value = replace_na(joe_value,0))

links <- ffscrapr::dp_playerids()


today_noms <- mfl_getendpoint(conn,"freeAgents") %>%
  pluck("content","freeAgents","leagueUnit","player") %>%
  tibble() %>%
  unnest_wider(1)

joined <- today_noms %>%
  inner_join(links, by = c("id" = "mfl_id")) %>%
  filter(!is.na(fantasypros_id)) %>%
  inner_join(values, by = c("fantasypros_id" = "fp_id")) %>%
  full_join(dlf, by = "merge_name") %>%
  filter(!is.na(fantasypros_id) | (
    name.y %in% c("2021.1.06", "2021.1.11", "2021.1.12", "2021.2.09", "2021.3.02","2021.3.03")))

joined %>% 
  select(name = name.y, pos = position.y, team, value, age = age.y) %>% 
  write_csv("temp.csv")


#Offensive Plays

joined <- mfl_roster %>%
  left_join(links, by = c("player_id" = "mfl_id")) %>%
  filter(!is.na(fantasypros_id)) %>%
  left_join(values, by = c("fantasypros_id" = "fp_id")) %>%
  full_join(dlf, by = "merge_name") %>%
  mutate(value = replace_na(value,0),
         pos.x = replace_na(pos.x,"PICK"),
         franchise_name = case_when(name.y == "2021.1.01" | name.y == "2021.2.04" ~ "Team Pikachu",
                                    name.y %in% c("2021.1.02", "2021.2.03", "2021.2.05") ~ "Team Ness",
                                    name.y == "2021.1.03" ~ "Team Simon Belmont",
                                    name.y == "2021.1.04" | name.y == "2021.2.07" ~ "Team Dr. Mario",
                                    name.y %in% c("2021.1.05", "2021.2.06", "2021.2.08") ~ "Team Mewtwo",
                                    name.y == "2021.1.06" ~ "Team Bowser",
                                    name.y == "2021.1.07" | name.y == "2021.2.10"~ "Team Kirby",
                                    name.y == "2021.1.08" | name.y == "2021.2.11" ~ "Team Ice Climbers",
                                    name.y == "2021.1.09" | name.y == "2021.2.12" ~ "Team Diddy Kong",
                                    name.y == "2021.1.10" | name.y == "2021.3.01" ~ "Team Donkey Kong",
                                    name.y == "2021.1.11" | name.y == "2021.3.02" ~ "Team Yoshi",
                                    name.y == "2021.1.12" | name.y == "2021.3.03" ~ "Team Link",
                                    name.y %in% c("2021.2.01", "2021.2.09", "2021.3.04") ~ "Team Luigi",
                                    name.y == "2021.2.02" | name.y == "2021.3.05" ~ "Team King Dedede",
                                    TRUE ~ franchise_name))

#Plot value
joined %>%
  ggplot(aes(x=pos.x, y=log(joe_value))) +
  theme_minimal() +
  ggbeeswarm::geom_quasirandom() +
  ggrepel::geom_label_repel(data = subset(joined,
                                          franchise_name == "Team Luigi"),
            aes(label = player_name),
            nudge_x = 0.4,
            segment.colour = "red")

joined %>%
  filter(!is.na(franchise_name)) %>%
  group_by(franchise_name) %>%
  mutate(franchise_sum = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(franchise_name = fct_reorder(franchise_name, franchise_sum)) %>%
  ggplot(aes(x=franchise_name, y=value, fill = pos.x, color = pos.x)) +
  theme_minimal() +
  geom_bar(position="stack", stat="identity")

temp <- joined %>%
  filter(franchise_name %in% c("Team Luigi", "Team Bowser", "Team Donkey Kong")) %>%
  select(franchise_name, merge_name, joe_value, value, age.x)
