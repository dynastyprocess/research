library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

headers = c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
  `Accept-Language` = "en-US,en;q=0.5",
  `Accept-Encoding` = "gzip, deflate, br",
  `Connection` = "keep-alive",
  `Upgrade-Insecure-Requests` = "1",
  `Sec-Fetch-Dest` = "document",
  `Sec-Fetch-Mode` = "navigate",
  `Sec-Fetch-Site` = "cross-site",
  `TE` = "trailers"
)

res <-
  httr::GET(url = "https://api.prop-odds.com/beta/games/mlb?date=2023-07-22&tz=America/New_York&api_key=8L4rHpqE74NXoqjUYtCavoPFTx05cGbPAe7WLCcJA",
            httr::add_headers(.headers = headers))

y <- content(res, as = "text") %>% jsonlite::parse_json()

games <- 
  tibble(games = y$games) |> 
  unnest_wider(col = games)

markets_res <- 
  httr::GET(url = "https://api.prop-odds.com/beta/markets/5bdf50b0b492bcdd0b5efcb03b851355?api_key=8L4rHpqE74NXoqjUYtCavoPFTx05cGbPAe7WLCcJA",
            httr::add_headers(.headers = headers))

market_content <- content(markets_res, as = "text") %>% jsonlite::parse_json()

markets <- 
  tibble(markets = market_content$markets) |> 
  unnest_wider(col = markets)

one_market_res <- 
  httr::GET(url = "https://api.prop-odds.com/beta/odds/5bdf50b0b492bcdd0b5efcb03b851355/total_over_under_alternate?api_key=8L4rHpqE74NXoqjUYtCavoPFTx05cGbPAe7WLCcJA",
            httr::add_headers(.headers = headers))

one_market_content <- content(one_market_res, as = "text") %>% jsonlite::parse_json()

one_market <- 
  tibble(markets = one_market_content$sportsbooks) |> 
  unnest_wider(col = markets) |> 
  unnest_wider(col = market) |> 
  unnest_longer(col = outcomes) |> 
  unnest_wider(outcomes)


################### Pinny

require(httr)


headers = c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0",
  `Accept` = "application/json",
  `Accept-Language` = "en-US,en;q=0.5",
  `Accept-Encoding` = "gzip, deflate, br",
  `Referer` = "https://www.pinnacle.com/",
  `Content-Type` = "application/json",
  `X-API-Key` = "CmX2KcMrXuFmNg6YFbmTxE0y9CIrOi0R",
  `X-Device-UUID` = "f301512b-c17a9a60-f7683e0e-7edc36d1",
  `X-Session` = "yLxHdJJPF5RVcRJBM4dveydxbGuDagUE",
  `Origin` = "https://www.pinnacle.com",
  `Connection` = "keep-alive",
  `Sec-Fetch-Dest` = "empty",
  `Sec-Fetch-Mode` = "cors",
  `Sec-Fetch-Site` = "same-site",
  `TE` = "trailers"
)

res <- httr::GET(url = "https://api.arcadia.pinnacle.com/0.1/wallet/balance",
                 httr::add_headers(.headers=headers))

cont <- content(res, as = "text") %>% jsonlite::parse_json()
