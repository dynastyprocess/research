suppressPackageStartupMessages({
  ## Database
  library(DBI)
  library(odbc)

  ## Scraping
  library(rvest)

  ## Processing
  library(tidyverse)
  library(here)
  library(V8)
  library(lubridate)

  # Signalling
  library(httr)
  library(rlang)
  library(glue)
  
  setwd(here::here())
  options(stringsAsFactors = FALSE)
})

scrape_ecr <- function() {
  aws_db <- DBI::dbConnect(odbc::odbc(), "dynastyprocess")

  teamIDs <- dbGetQuery(aws_db, "SELECT * FROM dp_teamids")
  
  dbDisconnect(aws_db)

  fp_pages <- .choose_pages(today())

  df_ecr <- fp_pages %>%
    select(fp_page = url, page_type, ecr_type) %>%
    mutate(data = map(fp_page, .scrape_page)) %>%
    hoist(
      data,
      "player" = "player_name",
      "id" = "player_id",
      "pos" = "player_position_id",
      "team" = "player_team_id",
      "ecr" = "rank_ave",
      "sd" = "rank_std",
      "best" = "rank_min",
      "worst" = "rank_max",
      "sportsdata_id",
      "player_filename",
      "yahoo_id" = "player_yahoo_id",
      "cbs_id" = "cbs_player_id",
      "player_owned_avg",
      "player_owned_espn",
      "player_owned_yahoo",
      "player_image_url",
      "player_square_image_url",
      "rank_delta" = "player_ecr_delta",
    ) %>%
    select(-data) %>%
    unnest(c(-page_type, -ecr_type, -fp_page)) %>%
    mutate(
      across(c("ecr", "sd", "best", "worst", starts_with("player_owned"), "rank_delta"), as.numeric),
      across(c(contains("id"),"ecr_type","fp_page", "player", "team", "pos", "player_filename", contains("image")), as.character),
      mergename = .add_mergename(player),
      scrape_date = today()
    ) %>%
    .add_mflteams(teamIDs) %>%
    .clean_idp_names() %>%
    filter(!is.na(sportsdata_id))
}

.choose_pages <- function(datestamp) {
  current_timeframe <- case_when(
    between(month(today()), 10, 12) | (month(today()) == 9 & day(today()) >= 10) ~ "3_inseason",
    month(today()) >= 8 & day(today()) >= 15 ~ "2_preseason",
    TRUE ~ "1_offseason"
  )

  fp_pages <- read.csv("fantasypros_pages_2.csv", fileEncoding = "UTF-8-BOM", na.strings = "") %>%
    filter(timeframe == current_timeframe)

  return(fp_pages)
}

.extract_ecrdatascript <- function(script_nodes, extractor = "ecrData") {
  x <- script_nodes %>%
    map(as.character) %>%
    str_detect(extractor)
  script_nodes[x]
}

.scrape_page <- function(page) {
  if (interactive()) message(page)

  page_js <- read_html(glue::glue("https://www.fantasypros.com/{page}")) %>%
    html_nodes("script") %>%
    .extract_ecrdatascript() %>%
    html_text()

  if (is.na(page_js)) {
    return(tibble())
  }

  ct <- V8::v8()

  ct$eval(page_js)
  
  ecr_data <- ct$get('ecrData')
  expert_groups <- ct$get('expertGroupsData')

  expert_data <- expert_groups$expert_data
  
  recent_experts <- expert_groups %>% 
    purrr::pluck('recency_groups','recency','options',1) %>%
    mutate(expert_count = map_dbl(experts,length),
           expert_pct = expert_count/max(expert_count)) %>% 
    arrange(expert_pct) %>% 
    filter(expert_pct>=0.05) %>%
    slice(1)
  
  if(interactive()) print(recent_experts$title)
  
  recent_experts <- recent_experts %>% 
    dplyr::pull('experts') %>% 
    unlist() %>% 
    paste(collapse = ":")

  query <- glue::glue("https://api.fantasypros.com/v2/json/nfl/2020/consensus-rankings?",
                      "type={ecr_data$ranking_type_name}&",
                      "scoring={ecr_data$scoring}&",
                      "position={ecr_data$position_id}&",
                      "week={ecr_data$week}&",
                      "filters={recent_experts}&",
                      "experts=available")
  
  x <- httr::GET(query,
    httr::add_headers(`x-api-key` = "zjxN52G3lP4fORpHRftGI2mTU8cTwxVNvkjByM3j")
  ) %>%
    httr::content(as = 'parsed') %>% 
    purrr::pluck('players') %>% 
    tibble() %>% 
    unnest_wider(1)
  
  return(ecr_data$players)
}

.clean_idp_names <- function(df_ecr) {
  x <- df_ecr %>%
    mutate(pos = case_when(
      pos %in% c("DE", "DT", "NT", "DL") ~ "DL",
      pos %in% c("OLB", "MLB", "LB") ~ "LB",
      pos %in% c("CB", "S", "DB") ~ "DB",
      TRUE ~ pos
    )) %>%
    filter(!(str_ends(.data$ecr_type, "p") & str_detect(.data$pos, .data$fp_page)))
}

.add_mergename <- function(Player) {
  Player %>%
    str_remove("( Jr.$)|( Sr.$)|( III$)|( II$)|( IV$)|( V$)|(\\')|(\\.)") %>%
    str_to_lower()
}

.add_mflteams <- function(dataframe, teamIDs) {

  # cleans up teamnames to the MFL standard, because #teamMFL

  dataframe %>%
    nest_join(teamIDs, by = c("team" = "fp")) %>%
    hoist(teamIDs, "tm" = "mfl") %>%
    select(-teamIDs)
}

ecr <- scrape_ecr()

ecr_pos <- ecr %>%
  select(sportradar_id = sportsdata_id, ecr_pos_combo_next = ecr, scrape_date, page_type) %>% 
  filter(page_type %in% c('weekly-qb','weekly-rb','weekly-wr','weekly-te')) %>%
  filter(scrape_date == max(scrape_date)) %>% 
  select(-scrape_date, -page_type)

ecr_ovr <- ecr %>%
  select(sportradar_id = sportsdata_id, ecr_ovr_combo_next = ecr, scrape_date, page_type) %>% 
  filter(page_type %in% c('weekly-op')) %>%
  filter(scrape_date == max(scrape_date)) %>% 
  select(-scrape_date, -page_type)

