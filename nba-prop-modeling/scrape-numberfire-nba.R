require(httr)
library(tidyverse)

headers = c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/119.0",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
  `Accept-Language` = "en-US,en;q=0.5",
  `Accept-Encoding` = "gzip, deflate, br",
  `Referer` = "https://www.numberfire.com/nba/fantasy/weekly-projections/",
  `DNT` = "1",
  `Connection` = "keep-alive",
  `Upgrade-Insecure-Requests` = "1",
  `Sec-Fetch-Dest` = "document",
  `Sec-Fetch-Mode` = "navigate",
  `Sec-Fetch-Site` = "same-origin",
  `Sec-Fetch-User` = "?1",
  `TE` = "trailers"
)

scrape_numberfire_date <- function(date){
  
  Sys.sleep(10)
  
  params = list(
    `scope` = "total",
    `d` = date
  )
  
  res <- httr::GET(url = "https://www.numberfire.com/nba/fantasy/weekly-projections/",
                   httr::add_headers(.headers=headers),
                   query = params)
  
  numberfire <- 
    content(res) |> 
    rvest::html_table()
  
  projections <- 
    tibble(name = numberfire[[1]]$Player) |> 
    bind_cols(numberfire[[2]])
  
  return(projections)
  
}

scrape_years <- 
  tibble(date = as_date("2013-10-01"):as_date("2023-11-11")) |> 
  mutate(date = as_date(date),
         day_of_week = wday(date, label = TRUE)) |> 
  filter(day_of_week == "Mon") |> 
  mutate(numberfire_nested = map(.x = date,
                                 .f = scrape_numberfire_date,
                                 .progress = TRUE))


 

scrape_years_unnest <- 
  scrape_years |> 
  mutate(how_many_rows = map_dbl(numberfire_nested, nrow)) |> 
  filter(how_many_rows > 1) |> 
  unnest(numberfire_nested) |> 
  separate(col = name,
           into = c("name", "name_short", "pos_team"),
           sep = "\n") |> 
  mutate(across(c(name, name_short, pos_team),
                .f = str_trim),
         pos_team = str_remove_all(pos_team, "\\(|\\)")) |> 
  separate(col = pos_team,
           into = c("pos","team"),
           sep = ", ") |> 
  mutate(across(c(`FG%`, `FT%`, `3P%`),
                .f = ~as.numeric(str_remove(.x, "%"))/100)) |> 
  janitor::clean_names() |> 
  select(-how_many_rows,
         -day_of_week)

#TODO filter out that Jason Terry 2016-01-18 line
write.csv(scrape_years_unnest, "numbefire_2013_2023.csv")
