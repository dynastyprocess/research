suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(RSQLite)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(lubridate)
  library(glue)
  library(magrittr)
  
  # Plotting
  library(ggimage)
  library(grid)
  library(ggrepel)
  library(nflfastR)
  
  # Shiny
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(reactable)
  library(gfonts)
  # library(DT)
  
})

source('fn_ui_desktop.R')
options(warn=0, dplyr.summarise.inform = FALSE)

# Create Functions --------------------------------------------------------

get_players_sorted <- function(data, var) {
  
  data %>%
    group_by(Name) %>% 
    summarise(total = sum(.data[[var]], na.rm = TRUE)) %>% 
    arrange(desc(total)) %>%
    select(Name) %>% 
    pull()
}

sticky_style <- function(left = TRUE) {
  style <- list(position = "sticky", background = "#f7f7f8", zIndex = 1)
  if (left) {
    style <- c(style, list(left = 0, borderRight = "1px solid #eee"))
  } else {
    style <- c(style, list(right = 0, borderLeft = "1px solid #eee"))
  }
  style
}

colClean <- function(x) {str_to_upper(gsub("_", " ", colnames(x), fixed = TRUE))}

# Import Data -------------------------------------------------------------

setwd(here::here())
epdata <- read_parquet("ep_1999_2019.pdata") %>% 
  filter(Season >= 2011)
vars <- epdata %>% select(contains("pass"), contains("rush"), contains("rec"), contains("total"), -contains("team"), -contains("proxy")) %>%
  colClean() %>% sort()
week_seasons <- epdata %>% arrange(Season, Week) %>% distinct(week_season) %>% as_vector()

week_master <- epdata %>%
  select(Season, Week, week_season, week_season_num) %>%
  distinct()

logos <- nflfastR::teams_colors_logos

season_data <- epdata %>%
  filter(Week <= 17) %>% 
  select(Season, Team, gsis_game_id, contains("team")) %>% 
  unique() %>% 
  group_by(Season,Team) %>% 
  summarise(games=n_distinct((gsis_game_id)),
            across(contains("team"), ~sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  inner_join(select(logos, team_abbr, team_logo_wikipedia),by=c("Team"="team_abbr"))

vars2 <- season_data %>% select(contains("team"), -pass_att_team, -pass_ay_team, -pass_comp_team, -Team, -team_logo_wikipedia,
                                -ends_with("x"), -ends_with("diff")) %>% colClean() %>% sort()

# UI section --------------------------------------------------------------
ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  title = "Expected Points - DynastyProcess.com",
  navbar = ui_header("Expected Points App"),
  sidebar = ui_sidebar(
    menuItem('Weekly EP',tabName = 'weekly',icon = 'chart-line'),
    menuItem('Data Tables',tabName = 'data', icon = 'table'),
    #menuItem('Trends',tabName = 'trends',icon = 'send'),
    menuItem('Team Level Trends',tabName = 'league',icon = 'trophy'),
    menuItem('About',tabName = 'about',icon = 'question-circle')
  ),
  dashboardBody(
    use_font("fira-sans-condensed", "www/css/fira-sans-condensed.css"),
    tags$style(HTML("
    #selectColBox {overflow:auto;}
                     ")),
    tabItems(
      tabItem(tabName = 'weekly',
              h1('Weekly Expected Points',style = "padding-left:10px;"),
              box(title = "Inputs",
                  status = "danger",
                  width = 12,
                  fluidRow(
                    column(width = 4,
                           selectizeInput("selectVar",
                                          "Select Variable:",
                                          choices = vars,
                                          selected = "TOTAL FP X"),
                           checkboxInput("pivot_trendlines", label = "Display Trendlines", value = TRUE)
                    ),
                    column(width = 4,
                           pickerInput("selectTeam",
                                       "Select Teams:",
                                       choices = sort(unique(epdata$Team)),
                                       selected = NULL,
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE)),
                           pickerInput("selectPos",
                                       "Select Positions:",
                                       choices = c("QB", "RB", "WR", "TE"),
                                       selected = c("RB","WR","TE"),
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE))
                    ),
                    column(width = 4,
                           pickerInput("selectSeason",
                                       "Select Seasons:",
                                       choices = rev(sort(unique(epdata$Season))),
                                       selected = c("2020"),
                                       multiple = TRUE),
                           pickerInput("selectPlayers",
                                       "Select Players:",
                                       choices = sort(unique(epdata$Name)),
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE),
                                       multiple = TRUE)
                    )
                    
                  )
              ),
              box(
                maximizable = TRUE,
                width = 12,
                fluidRow(width = 12,
                         plotOutput("pivotGraph", height = "35em"))),
              box(
                  width = 12,
                  fluidRow(width = 12,
                           reactableOutput("teamPivot", width = "100%")))
      ),
      tabItem(tabName = 'data',
              h1("Data Tables", style = "padding-left:10px;"),
              box(title = "Inputs",
                  status = "danger",
                  width = 12,
                  collapsible = FALSE,
                  fluidRow(
                    column(width = 5,
                           id = "selectColBox",
                           radioGroupButtons("selectCol",
                                             "Select Columns:",
                                             choices = c("Exp Points","Pass Stats","Rush Stats","Rec Stats","Total Stats","AY Stats"),
                                             selected = "Exp Points",
                                             # justified = TRUE,
                                             size = 'sm',
                                             status = "danger"),
                           conditionalPanel(
                             condition = "input.selectCol != 'AY Stats'",
                             radioGroupButtons("weeklyRadio",
                                               "Aggregate Data:",
                                               choices = c("Weekly","Weekly Average","Totals"),
                                               selected = "Weekly Average",
                                               size = 'sm',
                                               status = "danger"))),
                    column(width = 4,
                           pickerInput("selectTeam2",
                                       "Select Teams:",
                                       choices = sort(unique(epdata$Team)),
                                       selected = "SEA",
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE)),
                           pickerInput("selectPos2",
                                       "Select Positions:",
                                       choices = c("QB", "RB", "WR", "TE"),
                                       selected = c("RB","WR","TE"),
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE))),
                    column(width = 3,
                           pickerInput("selectSeason2",
                                       "Select Weeks:",
                                       choices = epdata %>% arrange(-Season, -Week) %>% select(week_season) %>% distinct() %>% pull(),
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE),                                       
                                       selected = epdata %>% filter(Season == "2020") %>% select(week_season) %>% distinct() %>% pull(),
                                       multiple = TRUE),
                           pickerInput("selectPlayers2",
                                       "Select Players:",
                                       choices = sort(unique(epdata$Name)),
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE),
                                       multiple = TRUE)))),
                    box(title = "Data",
                        status = "danger",
                        maximizable = TRUE,
                        width = 12,
                        fluidRow(
                          width = 12,
                          reactableOutput("table", width = "100%")))
              
      ),
      tabItem(tabName = 'league',
              h1("Team Level Trends", style = "padding-left:10px;"),
              box(title = "Inputs",
                  status = "danger",
                  width = 12,
                  fluidRow(
                    column(width=3,
                           selectizeInput("selectVar2",
                                          "Select Variable:",
                                          choices = vars2,
                                          selected = "PASS TD TEAM"),
                           conditionalPanel(
                             condition = "input.rate_stats",
                             selectizeInput("denomStat",
                                            "Divided by:",
                                            choices = c("GAMES","RUSH YD TEAM","RUSH ATT TEAM",
                                                        "PASS YD TEAM","PASS ATT TEAM","PASS COMP TEAM",
                                                        "REC YD TEAM","TOTAL YD TEAM"),
                                            selected = "PASS YD TEAM"))),
                    column(width=1,
                           checkboxInput("rate_stats", label = "Rate Stats", value = FALSE)),
                    column(width=4,
                           pickerInput("selectTeam3",
                                       "Select Teams:",
                                       choices = sort(unique(season_data$Team)),
                                       options = list(`actions-box` = TRUE,
                                                      `selected-text-format`= "count > 1",
                                                      `live-search` = TRUE,
                                                      `max-options` = 5),
                                       selected = c("SEA","GB","KC","BAL"),
                                       multiple = TRUE)),
                    column(width = 4,
                           pickerInput("selectSeason3",
                                       "Select Seasons:",
                                       choices = rev(sort(unique(season_data$Season))),
                                       selected = c("2017","2018","2019"),
                                       multiple = TRUE))
                    
                  )
              ),
              box(width = 12,
                  status = "danger",
                  title = "Plot",
                  #height = "150%",
                  fluidRow(width = 12,
                           plotOutput("leaguePlot", height = "50em")))
      ),
      tabItem(tabName='about',
              h1('About - Expected Points', style = "padding-left:10px"),
              box(status = "danger",
                  width = 12,
                  fluidRow(column(12,
                                  includeMarkdown('about_ep.md'))))
      )
    )
  )
)



# Server Section ----------------------------------------------------------

server <- function(input, output, session) {
  
  random_team <- sample(unique(epdata$Team),1)
  updatePickerInput(session,"selectTeam",selected = random_team)
  updatePickerInput(session,"selectTeam2",selected = random_team)

  inputVar <- reactive({str_to_lower(gsub(" ", "_", input$selectVar, fixed = TRUE))})
  
  weeklyEP <- reactive({
    epdata %>%
      filter(Team %in% input$selectTeam,
             Pos %in% input$selectPos,
             Season %in% input$selectSeason) %>%
      select(Season, Week, week_season, week_season_num, gsis_id, Name, Team, Pos, inputVar())

  })
  
  observeEvent({input$selectTeam
    input$selectPos
    input$selectSeason
    #inputVar()
    },{
      updatePickerInput(session, 'selectPlayers',
                           choices = get_players_sorted(weeklyEP(), inputVar()),
                           selected = head(get_players_sorted(weeklyEP(), inputVar()),5))
    })
  
  weeklyEP_playerfilter <- reactive({
    req(input$selectPlayers)
    
    weeklyEP() %>% 
      filter(Name %in% input$selectPlayers)
  })
  
  weekPivot <- reactive({
    weeklyEP_playerfilter() %>%
      arrange(week_season_num) %>%
      select(-week_season_num, -Week, -Season) %>%
      group_by(gsis_id) %>%
      mutate(Total = sum(.data[[inputVar()]], na.rm = TRUE),
             Average = mean(.data[[inputVar()]], na.rm = TRUE)) %>%
      pivot_wider(names_from = week_season,
                  values_from = inputVar()) %>%
      ungroup() %>%
      select(Name, Team, Pos, contains("Week"), Total, Average) %>%
      mutate_if(is.numeric, round, digits = 1) %>%
      arrange(desc(Average))
  })
  
  output$pivotGraph <- renderPlot({

    req(input$selectTeam)
    
    pivotgraph_data <- weeklyEP_playerfilter() %>%
      group_by(gsis_id) %>%
      mutate(player_weeks = n()) %>%
      ungroup() %>%
      group_by(Season, Week, week_season) %>%
      mutate(week_season_num = cur_group_id(),
             week_season_num_smooth = case_when(player_weeks > 2 ~ week_season_num,
                                         TRUE ~ 0L)) %>%
      ungroup() %>%
      mutate(week_season = forcats::fct_reorder(week_season, week_season_num))
    
    #plot_breaks <- unique(pivotgraph_data$week_season)
    
    pivotgraph_data %>% 
      ggplot(aes(x = week_season, y = .data[[inputVar()]], color = Name)) +
      geom_point(size = 3) +
      theme_bw() + 
      labs(x=element_blank(), y=input$selectVar, title=paste0("Weekly Summary \n",input$selectVar)) +
                                                #,": ", paste(input$selectTeam, collapse =',')," | ",input$selectPos)) +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      theme(plot.title = element_text(face='bold'),
            panel.spacing = unit(0,"lines"),
            text = element_text(size=18),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_y_continuous(limits = c(floor(min(pivotgraph_data[[inputVar()]])),
                                    ceiling(max(pivotgraph_data[[inputVar()]])))) +
      
      # scale_x_discrete(breaks = plot_breaks[seq(1, by = case_when(length(plot_breaks) < 22 ~ 2,
      #                                                             length(plot_breaks) < 44 ~ 4,
      #                                                             length(plot_breaks) < 66 ~ 8,
      #                                                             TRUE ~ 16),
      #                                           to = length(plot_breaks))]) +
      if(input$pivot_trendlines){geom_smooth(aes(x=week_season_num_smooth, y = .data[[inputVar()]]),
                                             method = "loess",
                                             #formula = y ~ splines::bs(x, 2),
                                             #formula = "y ~ poly(x, 3)",
                                             na.rm = TRUE,
                                             fill = NA)} 
  })
  
  get_season_weeks <- function(year){
    weekPivot() %>% 
      select(contains(year)) %>% 
      colnames()
  }
  
  create_colgroups <- function(years){
    bucket <- c()

    for (i in 1:length(years)) {
      bucket[[i]] <- colGroup(name = years[i], get_season_weeks(years[i]))
    }

    bucket
  }
  
  output$teamPivot <- renderReactable({
    weekPivot() %>% 
      reactable(
        defaultColDef = colDef(
          #header = function(value) gsub("\\,.*", "", value),
          header = function(value) str_extract(value, "\\d*?(?=,)"),
          #cell = function(value) format(value, nsmall = 1),
          align = "center",
          minWidth = 50
          #headerStyle = list(background = "#f7f7f8")
        ),
        columnGroups = create_colgroups(input$selectSeason),
        columns = list(
          Name = colDef(header = function(value) value,
                        style = sticky_style(),
                        minWidth = 100,
                        headerStyle = sticky_style()),
          Team = colDef(header = function(value) value),
          Pos = colDef(header = function(value) value),
          Total = colDef(header = function(value) value),
          Average = colDef(header = function(value) value,
                           style = sticky_style(left = FALSE),
                           minWidth = 85,
                           headerStyle = sticky_style(left = FALSE))
        ),
        bordered = TRUE,
        highlight = TRUE,
        #style = list(maxWidth = "100%"),
        #searchable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        compact = TRUE,
        width = "100%"
        #,width = 1750
      )
    
  })
  
  weeklyEP2 <- reactive({
    field_names <- switch(input$selectCol,
                          # "Exp Points" = rlang::exprs(ends_with("x")),
                          # "Pass Stats" = rlang::exprs(starts_with("pass")),
                          # "Rush Stats" = rlang::exprs(starts_with("rush")),
                          # "Rec Stats" = rlang::exprs(starts_with("rec")),
                          # "Total Stats" = rlang::exprs(starts_with("total")),
                          # "AY Stats" = rlang::exprs(starts_with("rec"), starts_with("total"), starts_with("pass"))
                          "Exp Points" = c("total_fp_x", "total_yd_x", "total_td_x",
                                           "pass_fp_x","pass_comp_x","pass_yd_x","pass_td_x",
                                           "rec_comp_x","rec_fp_x","rec_yd_x","rec_td_x",
                                           "rush_fp_x","rush_yd_x","rush_td_x"),
                          "Pass Stats" = c("pass_fp","pass_fp_x","pass_fp_diff",
                                           "pass_att","pass_comp","pass_comp_x","pass_comp_diff",
                                           "pass_ay","pass_yd","pass_yd_x","pass_yd_diff",
                                           "pass_td","pass_td_x","pass_td_diff"),
                          "Rush Stats" = c("rush_fp","rush_fp_x","rush_fp_diff",
                                           "rush_att","rush_yd","rush_yd_x","rush_yd_diff",
                                           "rush_td","rush_td_x","rush_td_diff"),
                          "Rec Stats" = c("rec_fp","rec_fp_x","rec_fp_diff",
                                          "rec_tar","rec_comp","rec_comp_x","rec_comp_diff",
                                          "rec_ay","rec_yd","rec_yd_x","rec_yd_diff",
                                          "rec_td","rec_td_x","rec_td_diff"),
                          "Total Stats" = c("total_fp","total_fp_x","total_fp_diff",
                                            "total_yd","total_yd_x","total_yd_diff",
                                            "total_td","total_td_x","total_td_diff"),
                          "AY Stats" = c("rec_comp","rec_tar","rec_yd","rec_ay","rec_td",
                                         "pass_ay_team","pass_att_team",
                                         "total_fp_x","total_fp_team_x",
                                         "total_fp","total_fp_team"))
    
    arrange_field <- switch(input$selectCol,
                          "Exp Points" = rlang::exprs(total_fp_x),
                          "Pass Stats" =  rlang::exprs(pass_fp_x),
                          "Rush Stats" =  rlang::exprs(rush_fp_x),
                          "Rec Stats" =  rlang::exprs(rec_fp_x),
                          "Total Stats" =  rlang::exprs(total_fp_x),
                          "AY Stats" = rlang::exprs(WOPR))
    
    epdata %>%
      filter((Team %in% input$selectTeam2),
             (Pos %in% input$selectPos2),
             week_season %in% input$selectSeason2) %>%
      {if (input$selectCol == "AY Stats")
        select(., Name, Season, Week, Team, Pos, field_names)
        else select(., Name, Season, Week, Team, Pos, sort(field_names),
                    -contains("_team"), -contains("proxy"))} %>%
      {if (input$selectCol == "AY Stats")
        group_by(., Name, Team, Pos) %>% 
          summarise(games = n(),
                    rec_comp = sum(rec_comp, na.rm = TRUE),
                    rec_tar =  sum(rec_tar, na.rm = TRUE),
                    rec_yd = sum(rec_yd, na.rm = TRUE),
                    rec_ay = sum(rec_ay, na.rm = TRUE),
                    rec_td = sum(rec_td, na.rm = TRUE),
                    rec_adot = sum(rec_ay, na.rm = TRUE) / sum(rec_tar, na.rm = TRUE),
                    rec_ay_share = sum(rec_ay, na.rm = TRUE) / sum(pass_ay_team, na.rm = TRUE),
                    rec_tgt_share = sum(rec_tar, na.rm = TRUE) / sum(pass_att_team, na.rm = TRUE),
                    WOPR = 1.5*sum(rec_tar, na.rm = TRUE) / sum(pass_att_team, na.rm = TRUE) + 0.7*sum(rec_ay, na.rm = TRUE) / sum(pass_ay_team, na.rm = TRUE),
                    RACR = sum(rec_yd, na.rm = TRUE) / sum(rec_ay, na.rm = TRUE),
                    YPTPA = sum(rec_yd, na.rm = TRUE) / sum(pass_att_team, na.rm = TRUE),
                    total_fp_x_share = sum(total_fp_x, na.rm = TRUE) / sum(total_fp_team_x, na.rm = TRUE),
                    total_fp_share = sum(total_fp, na.rm = TRUE) / sum(total_fp_team, na.rm = TRUE))
        else if (input$weeklyRadio == "Weekly Average")
          group_by(., Name, Team, Pos) %>%
          summarise(games = n(),
                    across(field_names, ~mean(.x, na.rm = TRUE))) 
        else if (input$weeklyRadio == "Totals")
          group_by(., Name, Team, Pos) %>%
          summarise(games = n(),
                    across(field_names, ~sum(.x, na.rm = TRUE)))
        else .} %>% 
      ungroup() %>% 
      {if (input$selectCol == "AY Stats")
        mutate(., across(where(is.numeric), ~round(.x, 2)))
        else mutate(., across(where(is.numeric), ~round(.x, 1))) } %>% 
      arrange(-(!!!arrange_field))
    
  })
  
  observeEvent({input$selectTeam2
    input$selectPos2
    input$selectSeason2},{
      updatePickerInput(session, 'selectPlayers2',
                        choices = weeklyEP2()$Name,
                        selected = weeklyEP2()$Name)
    })
  
  weeklyEP2_playerfilter <- reactive({
    req(input$selectPlayers2)
    
    weeklyEP2() %>% 
      filter(Name %in% input$selectPlayers2)
  })
  
  output$table <- renderReactable({
    
    colGroupSwitch <- switch(input$selectCol,
                          "Exp Points" = list(
                            colGroup(name = "Total Expected", columns = c("total_fp_x", "total_yd_x", "total_td_x")),
                            colGroup(name = "Expected Passing", columns = c("pass_fp_x","pass_comp_x","pass_yd_x","pass_td_x")),
                            colGroup(name = "Expected Receiving", columns = c("rec_comp_x","rec_fp_x","rec_yd_x","rec_td_x")),
                            colGroup(name = "Expected Rushing", columns = c("rush_fp_x","rush_yd_x","rush_td_x"))),
                          "Pass Stats" = list(
                            colGroup(name = "Fantasy Points", columns = c("pass_fp","pass_fp_x","pass_fp_diff")),
                            colGroup(name = "Completions", columns = c("pass_comp","pass_comp_x","pass_comp_diff")),
                            colGroup(name = "Pass Yards", columns = c("pass_ay","pass_yd","pass_yd_x","pass_yd_diff")),
                            colGroup(name = "Touchdowns", columns = c("pass_td","pass_td_x","pass_td_diff"))),
                          "Rush Stats" = list(
                            colGroup(name = "Fantasy Points", columns = c("rush_fp","rush_fp_x","rush_fp_diff")),
                            colGroup(name = "Rush Yards", columns = c("rush_yd","rush_yd_x","rush_yd_diff")),
                            colGroup(name = "Touchdowns", columns = c("rush_td","rush_td_x","rush_td_diff"))),
                          "Rec Stats" = list(
                            colGroup(name = "Fantasy Points", columns = c("rec_fp","rec_fp_x","rec_fp_diff")),
                            colGroup(name = "Catches", c("rec_comp","rec_comp_x","rec_comp_diff")),
                            colGroup(name = "Rec Yards", c("rec_ay","rec_yd","rec_yd_x","rec_yd_diff")),
                            colGroup(name = "Touchdowns", c("rec_td","rec_td_x","rec_td_diff"))),
                          "Total Stats" = list(
                            colGroup(name = "Fantasy Points", columns = c("total_fp","total_fp_x","total_fp_diff")),
                            colGroup(name = "Yards", columns = c("total_yd","total_yd_x","total_yd_diff")),
                            colGroup(name = "Touchdowns", columns = c("total_td","total_td_x","total_td_diff")))
    )
    
    weeklyEP2_playerfilter() %>% 
      reactable(
        defaultColDef = colDef(
          #header = function(value) str_to_upper(gsub("_", " ", value, fixed = TRUE)),
          header = function(value) case_when(input$selectCol == "Exp Points" & str_detect(value, "fp") ~ "FPs",
                                             input$selectCol == "Exp Points" & str_detect(value, "yd") ~ "Yards",
                                             input$selectCol == "Exp Points" & str_detect(value, "td") ~ "TDs",
                                             input$selectCol == "Exp Points" & str_detect(value, "comp") ~ "Comp",
                                             input$selectCol != "AY Stats" & str_detect(value, "_x") ~ "Exp",
                                             input$selectCol != "AY Stats" & str_detect(value, "_diff") ~ "Diff",
                                             input$selectCol != "AY Stats" & str_detect(value, "ay") ~ "AYs",
                                             input$selectCol != "AY Stats" & str_detect(value, "att") ~ "Attempts",
                                             input$selectCol != "AY Stats" & str_detect(value, "tar") ~ "Tar",
                                             input$selectCol != "AY Stats" & !(value %in% c("Name","Games","Week","Team","Pos","Season")) ~ "Actual",
                                             input$selectCol == "AY Stats" ~ str_to_upper(gsub("_", " ", value, fixed = TRUE)),                            
                                             TRUE ~ value),
          cell = function(value) format(value, nsmall = 1),
          align = "center",
          minWidth = 81,
          #headerStyle = list(background = "#f7f7f8")
        ),
        columns = list(
          Name = colDef(style = sticky_style(),
                        minWidth = 100,
                        headerStyle = sticky_style()),
          games = colDef(name = "Games", cell = function(value) format(value, nsmall = 0)),
          Season = colDef(cell = function(value) format(value, nsmall = 0)),
          Week = colDef(cell = function(value) format(value, nsmall = 0))
          
        ),
        columnGroups = colGroupSwitch,
        bordered = TRUE,
        highlight = TRUE,
        #searchable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        compact = TRUE,
        width = "100%"
      )
  })
  
  inputVar2 <- reactive({str_to_lower(gsub(" ", "_", input$selectVar2, fixed = TRUE))})
  
  inputDenom <- reactive({str_to_lower(gsub(" ", "_", input$denomStat, fixed = TRUE))})
  
  output$leaguePlot <- renderPlot({
    asp_ratio <- 1.618
    #aspect.ratio = 1/asp_ratio
    
    season_data %>% 
      filter(Season %in% input$selectSeason3,
             Team %in% input$selectTeam3) %>% 
      {if (input$rate_stats) ggplot(., aes(.data[[paste0(inputVar2(),'_x')]] / .data[[inputDenom()]],
                                          .data[[inputVar2()]]  / .data[[inputDenom()]],
                                          group = Team))
        else ggplot(., aes(.data[[paste0(inputVar2(),'_x')]], .data[[inputVar2()]], group = Team)) } +
      geom_image(aes(image = team_logo_wikipedia), size = 0.05, by = "width", asp = asp_ratio) +
      geom_text_repel(aes(label = Season),force = 15, size=6, point.padding = 1.5) +
      #geom_point(aes(color=as.factor(Team), size=as.factor(Season))) +
      #geom_path() +
      geom_abline() +
      theme_bw() +
      #coord_fixed(ratio = 1) +
      theme(
        # panel.grid.major.y = element_blank(),
        # axis.text.y = element_blank(),
        aspect.ratio = 1/asp_ratio,
        plot.title = element_text(face='bold'),
        panel.spacing = unit(0,"lines"),
        text = element_text(size=20)) +
      {if (input$rate_stats) labs(x=paste("EXPECTED",input$selectVar2,'/',input$denomStat),
                                  y=paste(input$selectVar2,'/',input$denomStat),
                                  title=paste0("Yearly Trends \n",paste(input$selectVar2,'/',input$denomStat)))
        else labs(x=paste("EXPECTED",input$selectVar2), y=input$selectVar2, title=paste0("Yearly Trends \n",input$selectVar2)) } +
      annotation_custom(textGrob("Underperformed",x=0.95, y=0.1, hjust=1, vjust=0,
                                 gp=gpar(col="black", fontsize=40, fontface="bold", alpha = 0.15))) +
      annotation_custom(textGrob("Overperformed",x=0.05, y=0.9, hjust=0, vjust=1,
                                 gp=gpar(col="black", fontsize=40, fontface="bold", alpha = 0.15)))
    
    
  })
}

# thematic_shiny(font = "auto")
shinyApp(ui, server)
