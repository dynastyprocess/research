library(ffscrapr)
library(tidyverse)

#League IDs
# 22627 48
# 52290 AA

#Connection
conn <- mfl_connect(2020, 22627, user_agent = "dynastyprocess", rate_limit_number = 30, rate_limit_seconds = 60)

mfl_roster <- ffscrapr::ff_rosters(conn)

mfl_points <- tibble()

#Scrape whole year
for (i in 1:16) {mfl_points <- mfl_points %>% rbind(ffscrapr::ff_playerscores(conn, season = 2020, week = i))}

mfl_points_summ <- mfl_points %>% 
  group_by(player_id) %>% 
  summarise(avg_pts = mean(as.numeric(points), na.rm = TRUE),
            games = n()) %>% 
  ungroup()

values <- ffscrapr::dp_values()

links <- ffscrapr::dp_playerids()

#functions
get_dead_cap <- function(salary, years){
  running_salary <- salary
  running_dead_cap <- 0
  
  if (years == 0) return(0) else if (years == 1 & salary >= 10) return(salary *.25) else if (years == 1 & salary < 10) return(0)
  
  for(i in c(1:years)) {
    if (i>1) {running_salary <- running_salary * 1.1}
    
    if (running_salary >= 10) {running_dead_cap <- running_dead_cap + .25*running_salary }
    
  }
  
  running_dead_cap
  
}
  
#Offensive Plays
joined <- mfl_roster %>% 
  left_join(links, by = c("player_id" = "mfl_id")) %>%
  filter(!is.na(fantasypros_id)) %>% 
  left_join(values, by = c("fantasypros_id" = "fp_id")) %>%
  left_join(mfl_points_summ, by = "player_id") %>%
  mutate(salary = 1.1*as.numeric(salary),
         contractYear = as.numeric(contractYear) - 1,
         valuePerDollar = value_2qb / salary,
         avg_pts_per_dollar = avg_pts / salary,
         deadCap = map2_dbl(salary, contractYear, get_dead_cap),
         across(where(is.numeric), ~round(.x,2))) %>% 
  select(franchise_name, player_name, pos.x, salary, value_2qb, valuePerDollar, avg_pts_per_dollar, avg_pts,
         contractYear, deadCap) #%>% 
  #filter(deadCap > 0, valuePerDollar <= 80, contractYear > 0)

#Plot value
joined %>% 
  filter(pos.x %in% c("QB","RB","WR","TE"), contractYear > 0) %>% 
  ggplot(aes(x=pos.x, y=log(avg_pts))) +
  theme_minimal() +
  ggbeeswarm::geom_quasirandom(aes(size = salary)) +
  ggrepel::geom_label_repel(data = subset(joined,
                                          franchise_name == "Indigo Plateau Elite" &  contractYear > 0 &
                                          pos.x %in% c("QB","RB","WR","TE")),
            aes(label = player_name),
            nudge_x = 0.4,
            segment.colour = "red")

#IDP players
idp_plot <- mfl_roster %>% 
  filter(pos %in% c("DT","DE","LB","S","CB")) %>% 
  left_join(mfl_points_summ, by = "player_id") %>%
  mutate(salary = 1.1*as.numeric(salary),
         contractYear = as.numeric(contractYear) - 1,
         avg_pts_per_dollar = avg_pts / salary,
         deadCap = map2_dbl(salary, contractYear, get_dead_cap),
         across(where(is.numeric), ~round(.x,2))) %>% 
  select(franchise_name, player_name, pos, salary, contractYear, deadCap, avg_pts, games, avg_pts_per_dollar)

#Plot value
idp_plot %>% 
  filter(contractYear > 0) %>% 
  ggplot(aes(x=pos, y=log(avg_pts_per_dollar))) +
  theme_minimal() +
  ggbeeswarm::geom_quasirandom(aes(size = salary)) +
  #ggridges::geom_density_ridges(jittered_points = TRUE) +
  ggrepel::geom_label_repel(data = subset(idp_plot,
                                          franchise_name == "Indigo Plateau Elite" &  contractYear > 0),
                            aes(label = player_name),
                            nudge_x = 0.4,
                            #box.padding = 0.5,
                            segment.colour = "red")

#Plot ppg
idp_plot %>% 
  filter(contractYear > 0, avg_pts >= 6) %>% 
  ggplot(aes(x=pos, y=log(avg_pts))) +
  theme_minimal() +
  ggbeeswarm::geom_quasirandom(aes(size = salary)) +
  #ggridges::geom_density_ridges(jittered_points = TRUE) +
  ggrepel::geom_label_repel(data = subset(idp_plot,
                                          franchise_name == "Indigo Plateau Elite" &  contractYear > 0 & avg_pts >= 6),
                            aes(label = player_name),
                            nudge_x = 0.4,
                            #box.padding = 0.5,
                            segment.colour = "red")
  