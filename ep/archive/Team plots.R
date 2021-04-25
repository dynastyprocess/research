library(ggplot2)
library(ggrepel)
library(tidyverse)

plotdf <- df2019 %>%
  select(week, eTeamFP, TeamFP, posteam) %>%
  distinct()


ggplot(plotdf, aes(x=eTeamFP, y=TeamFP, label = posteam, color = as.factor(plotdf$week))) +
  geom_point() + 
  geom_abline() +
  geom_text_repel() +
  theme_bw()

plotdf2 <- df2019 %>%
  select(week, eTeamFP, TeamFP, posteam) %>%
  distinct() %>%
  #filter(week <= 4) %>%
  group_by(posteam) %>%
  summarise(eTeamFP = mean(eTeamFP),
            TeamFP = mean(TeamFP))
  


ggplot(plotdf2, aes(x=eTeamFP, y=TeamFP, label = posteam)) +
  geom_point() + 
  geom_abline() +
  geom_text_repel() +
  theme_bw()

summary(lm(TeamFP ~ eTeamFP, data=plotdf))
summary(lm(TeamFP ~ eTeamFP, data=plotdf2))
