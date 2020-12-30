# Libraries ----------------------------------------------------------------
library(tidyverse)
library(tidymodels)

library(skimr)
library(GGally)

pfr_idp <- read_csv("https://raw.githubusercontent.com/DynastyProcess/db/master/pfr/pfr_adv_defense.csv?token=ALJVRQMEGTBHS4IPSQHABK27HMZTS")

pfr_idp_clean <- pfr_idp %>%
  mutate(cmp_percent = as.numeric(str_remove(cmp_percent, "%")),
         m_tkl_percent = as.numeric(str_remove(m_tkl_percent,"%")))

skim(pfr_idp_clean)

pfr_idp_clean %>%
  select(int, tgt, cmp, cmp_percent, yds, yds_cmp, yds_tgt, td, rat, dadot, air, yac, comb, m_tkl, m_tkl_percent) %>%
  #select(-c(player, tm, pos, id_pfr, names, year, rk, age, g, gs)) %>%
  ggpairs()