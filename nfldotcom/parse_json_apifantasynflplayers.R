library(jsonlite)
library(tidyverse)
library(arrow)

nfl_json <- read_json("nfl_dotcom_fantasy_api.json")

df_nfljson <- nfl_json %>%
  pluck("data") %>%
  tibble() %>%
  unnest_wider(1) %>%
  unnest_wider("attributes") %>%
  unnest_wider("externalIds") %>%
  unnest_wider("legacyIds") %>%
  select(-relationships)

write_parquet(df_nfljson,"api-fantasy-nfl-com_players.parquet")
