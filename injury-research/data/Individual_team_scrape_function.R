###Load our packages
library(rvest)
library(stringr)
library(tidyverse)
library(tidyr)
library(glue)
library(dplyr)


team_injury_report <- function(team_abbr, season){
  
  ###URL of interest
  url <- read_html(glue("https://www.pro-football-reference.com/teams/",{team_abbr},"/",{season},"_injuries.htm"))
  
  ###Get roster of team from page along with season and team name
  Rosters <- url %>%
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "left", " " ))]') %>%
    html_text() %>%
    as.data.frame() %>%
    slice(1:(n()-1)) %>%
    slice(-1) %>%
    mutate(Team = url %>%
             html_nodes(xpath = '//*[@id="meta"]/div[2]/h1/span[2]') %>%
             html_text(),
           Season = url %>%
             html_nodes(xpath = '//*[@id="meta"]/div[2]/h1/span[1]') %>%
             html_text()) %>%
    rename("Names"=".")%>%
    mutate("Row" = row_number()) %>%
    relocate(Row, .before = Names)
  
  ###Create blank data frames to move data into
  test_frame  <- data.frame(matrix(NA,nrow(Rosters), 20))
  test_frame1 <- data.frame(matrix(NA,nrow(Rosters), 20))
  test_frame2 <- data.frame(matrix(NA,nrow(Rosters), 20))
  
  ###Iterate through the number of columns and games. The max number of games is 20 so we set it as 1:20
  for (x in 1:nrow(Rosters)) {
    
    for (y in seq_along(1:20)) {
      
      ###Direct url of specific team to xpath that contains info about injury, active status, and week
      team_url <- url %>%
        html_nodes(xpath= paste('//*[@id="team_injuries"]/tbody/tr[',x,']/td[',y,']'))
      
      ###Active or inactive
      test_frame[[x,y]] <- try(xml_attrs(team_url[[1]])[["class"]])
      test_frame <- test_frame
      
      ###Week information
      test_frame1[[x,y]] <- try(xml_attrs(team_url[[1]])[["data-stat"]])
      test_frame1 <- test_frame1
      
      ###Injury info
      test_frame2[[x,y]] <- try(xml_attrs(team_url[[1]])[["data-tip"]])
      test_frame2 <- test_frame2
      
      ###Bind our rosters with the 3 pieces of info from above
      df1 <- cbind(Rosters, test_frame)
      df2 <- cbind(Rosters, test_frame1)
      df3 <- cbind(Rosters, test_frame2)
      
      ###Make data longer for easier interpretation and rename variables
      melt_1 <- reshape::melt(df1, id.vars=c("Row","Names", "Team", "Season"))%>%
        rename("Active_Inactive"=value)
      melt_2 <- reshape::melt(df2, id.vars=c("Row","Names", "Team", "Season")) %>%
        select(value) %>%
        rename("Week"=value)
      melt_3 <- reshape::melt(df3, id.vars=c("Row","Names", "Team", "Season")) %>%
        select(value) %>%
        rename("Injury"=value)
      
      ###If DNP is detected in the string then individual did not play, rename output to reflect this rather than long code
      melt_1 <- melt_1 %>%
        mutate("Active_Inactive" = ifelse(str_detect(melt_1$Active_Inactive, "dnp") == TRUE, "Out", "Active")) 
      
      ###If the individual carried no designation then the data tip xpath returns an error, we detect this and replace the 
      output with a healthy designation but leave actual injury if one is available. Also, seperate string to create new column and specific injury.
      melt_3 <- melt_3 %>%
        mutate("Game_Status" = ifelse(str_detect(melt_3$Injury, "subscript out of bounds") == TRUE, "Healthy", Injury)) %>%
        select(-Injury) %>%
        separate(Game_Status, c("Game_Designation", "Injury_Type"), ":") 
      
      ###Fix blank values from above fix
      melt_3[melt_3==" "]<-NA
      melt_3[is.na(melt_3)]<-"None"
      
      ###Combine the data collected from above and remove uneeded columns, also remove the week_ text to just show week number
      final <- cbind(melt_1,melt_2, melt_3) %>%
        select(-Row, -variable) %>%
        mutate(Week = str_remove_all(Week, "week_")) 
      
      ###Make weeks numberic 
      final$Week <- as.numeric(final$Week)
      final$Injury_Type = str_to_title(final$Injury_Type)
      
      
      ###Filter out any NAs or weeks that team did not play in
      final <- final %>%
        filter(Week < 100)
      
    }
  }
  return(final)
}

###Note this may take a few minutes, html_table works to scrape the PFR table but misses valuable injury info that we need. 

team_injury_report("nwe",2020) 



```