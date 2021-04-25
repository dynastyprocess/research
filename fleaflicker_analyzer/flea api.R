library(tidyverse)
library(magrittr)
library(purrr)
library(jsonlite)

library(DT)
library(lubridate)
library(RColorBrewer)


roster_single <- fromJSON('https://www.fleaflicker.com/api/FetchRoster?league_id=206154&team_id=1373475') %>% extract2('groups') %>% as_tibble

rosters <- fromJSON('https://www.fleaflicker.com/api/FetchLeagueRosters?league_id=206154') %>% 
  extract2('rosters') %>% 
  as_tibble() %>% 
  unnest_wider('players') %>%
  hoist(proPlayer,
        'player_id'='id',
        'player'='nameFull',
        'team' = 'proTeamAbbreviation',
        'pos' = 'position') %>%
  hoist(owner,
        'owner_id'='id',
        'owner_name'='name'
  ) %>% 
  select(starts_with('owner_'),starts_with('player'),pos,team) %>% 
  unnest(cols = names(.)) %>%
  mutate(player = case_when(player == "Christopher Herndon" ~ "Chris Herndon",
                            TRUE ~ player),
         merge_name = player,
         team = case_when(team == "SF" ~ "SFO",
                          team == "LV" ~ "LVR",
                          team == "TB" ~ "TBB",
                          team == "NO" ~ "NOS",
                          team == "NE" ~ "NEP",
                          team == "GB" ~ "GBP",
                          team == "KC" ~ "KCC",
                          TRUE ~ team)
  ) %>%
  mutate_at("merge_name",str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('merge_name',str_squish) %>%
  mutate_at('merge_name',tolower)

players_raw <- read.csv("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values-players.csv") %>% 
  mutate(merge_name = player,
         player = paste0(player,", ",pos," ",team)) %>%
  mutate_at("merge_name",str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('merge_name',str_squish) %>%
  mutate_at('merge_name',tolower)

rookies_raw <- read.csv("https://raw.githubusercontent.com/tanho63/dynastyprocess/master/files/values-picks.csv") %>% 
  filter(str_detect(player,'2020 Pick')) %>% 
  rownames_to_column(var = 'pick') %>% 
  mutate(pick=as.numeric(pick))

rosters_values <- rosters %>%
  full_join(players_raw, by = c("merge_name","team","pos")) #%>%
  #filter(is.na(owner_name))

pivot_dpos <- rosters_values %>%
  group_by(owner_name,pos) %>%
  summarize(value = sum(value_1qb, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(owner_name) %>%
  mutate(total=sum(value,na.rm=TRUE)) %>%
  ungroup() %>%
  spread(pos,value)%>%
  gather(pos,value,total:WR)%>%
  group_by(pos) %>% mutate(total = sum(value,na.rm=TRUE)) %>% ungroup()%>%
  transmute(owner = owner_name, pos = pos, pct = round(value/total,digits=4))  %>%
  spread(pos,pct) %>%
  select(owner,qb=QB,rb=RB,wr=WR,te=TE,total)%>%
  arrange(desc(total))%>%
  mutate_all(~replace(.,is.na(.),0))

colors<-colorRampPalette(brewer.pal(3,'PuOr'))

brks_dpos<-function(colnum){
  breakvalue<-quantile(range(pivot_dpos[colnum]),probs=seq(0.01,0.99,0.01),na.rm=TRUE)
  return(breakvalue)
}

dt_dpos<-datatable(pivot_dpos,
                   rownames=FALSE,
                   colnames=c("Owner","QB","RB","WR","TE","Total"),
                   options(
                     paging=FALSE,
                     searching=FALSE
                   ))%>%
  formatPercentage(c(2:ncol(pivot_dpos)),2)

for (i in 2:ncol(pivot_dpos)){
  dt_dpos<-dt_dpos%>%
    formatStyle(i, backgroundColor = styleInterval(brks_dpos(i),colors(length(brks_dpos(i))+1)))
}
dt_dpos
  
joe_values <- rosters_values %>%
filter(owner_name == "Goldenrod City Nightmares" | owner_name == "Winterfell Dire Wolves")