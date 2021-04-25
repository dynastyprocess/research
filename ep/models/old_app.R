library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(nflscrapR)
library(DT)
library(here)
library(shinyWidgets)
library(ggplot2)

df2019 <- read.csv("data2019cleaned2.csv")

ui <- dashboardPage(
  skin="blue",
  title="DynastyProcess Apps: Expected Points",
  {dashboardHeader(title = a(href="https://dynastyprocess.com",
                            img(src = "logo-horizontal.png",
                                width='100%')),
                  titleWidth = 250)},
  {dashboardSidebar(width = 250,
                   sidebarMenu(
                     id='maintabs',
                     menuItem('Database', tabName = 'ep_table', icon = icon('table')),
                     menuItem('Weekly Breakdowns', tabName = 'ep_week', icon = icon('chart-line')),
                     menuItem('Buy Lows: Tan', tabName = 'buylows_tan',icon=icon('money-check-alt')),
                     menuItem('About EP',tabName='ep_about',icon=icon('question-circle'))
                     ),
                   sidebarMenu(
                     menuItem('Inputs:',startExpanded = TRUE,
                     selectizeInput("selectTeam",
                                 "Select Team:",
                                 choices = c("All", as.character(sort(unique(df2019$posteam)))),
                                 selected = "KC"),
                     selectizeInput("selectPos",
                                 "Select Position:",
                                 choices = c("All", "QB", "RB", "WR", "TE"),
                                 selected = "All"),
                     selectizeInput("selectPlayers",
                                    "Select Players:",
                                    choices = c("All"),
                                    selected = "All",
                                    multiple = TRUE),
                     radioButtons("weeklyRadio",
                                  "Weekly or Cumulative?",
                                  choices = c("Weekly","Cumulative"),
                                  selected = "Cumulative"),
                     conditionalPanel(condition = "input.weeklyRadio == 'Weekly'",
                                      selectizeInput("selectWeeks",
                                                     "Select Weeks:",
                                                     choices = c("All", sort(unique(df2019$week))),
                                                     selected = "All",
                                                     multiple = TRUE)),
                     #actionButton('updatefilter','Update!',class='btn-info'),
                     br()
                     )                     
                   ),
                   {sidebarMenu(
                     menuItem("More from DynastyProcess:", icon=icon("rocket"),
                              menuSubItem("Calculator",icon=icon('calculator'),href="https://apps.dynastyprocess.com/calculator"),
                              menuSubItem("Database",icon=icon('database'),href="https://apps.dynastyprocess.com/database"),
                              menuSubItem('Crystal Ball',icon=icon('quidditch'),href='https://apps.dynastyprocess.com/crystalball'),
                              #menuSubItem('Expected Points', icon=icon('chart-line'),href='https://apps.dynastyprocess.com/ep'),
                              menuSubItem("GitHub",icon=icon('github'),href="https://apps.dynastyprocess.com/ecr"),
                              menuSubItem("More!", icon=icon('rocket'),href="https://dynastyprocess.com/apps")
                     )
                   )}
  )},
  dashboardBody(
    {tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/flatly.css"),
      tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                  background-color: #000;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                  background-color: #555;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                  background-color: #000;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                  background-color: #000;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                  background-color: #555;
                                  text-decoration: none;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .sidebar-menu>li>.treeview-menu {
                                background-color: #111;
                                text-decoration: none;
                                }

                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                  background-color: #555;
                                  text-decoration: none;
                                }
                                .skin-blue .sidebar-menu > li.active > a{
                                  border-left-color: #fff
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                  background-color: #fff;
                                }
                                .btn {
                                  font-size: 12px;
                                }

                                .selectize-input
                                {font-size:12px;
                                min-height:25px;
                                padding-top:0px;
                                padding-bottom:0px;
                                }
                                      ')
      )
    )},
    tabItems(
      tabItem(tabName = 'ep_table',
              titlePanel('DynastyProcess Apps: Expected Points'),
              br(),
              # fluidRow(column(12,'You can change the inputs from the sidebar!')),
              # fluidRow(includeMarkdown('about.md')),
              fluidRow(column(12,
                    radioGroupButtons("selectCol","Select Columns:", choices = c("Exp Points","Rush Stats","Rec Stats","Total Stats","Rate Stats"),
                                      selected = "Exp Points")#,
                    #checkboxInput("datatable_filters", label = "Display Filters", value = FALSE)
                    )

              ),
              fluidRow(box(width = 12,
                           DTOutput("teamTable")))
      ),
      tabItem(tabName = "ep_week",
              titlePanel('DynastyProcess Apps: Expected Points'),
              fluidRow(box(width=12,status='info',"(You'll need to select 'weekly' instead of 'cumulative' in the sidebar to use this tab!)")),
              #fluidRow(includeMarkdown('about2.md')),
              fluidRow(box(width = 3,
                           selectInput("selectVar",
                                       "Select Variable:",
                                       choices = c("eFP","FP","Diff", "eFPshare","FPshare",
                                                   "eRushFP","eTeamRushFP","RushFP","TeamRushFP","RushDiff","Rushes","RushGames", "RushYD",
                                                   "eRecFP","eTeamRecFP","RecFP","TeamRecFP","RecDiff","Targets","TeamTargets",
                                                   "Catches","AYs","TeamAYs","RecYD","aDOT","RecGames","RushTD","RecTD","TD",     
                                                   "eTeamFP","TeamFP","TeamDiff","AYshare","TargetShare","WOPR","RACR","YPTPA"),
                                       selected = "eFP"),
                           checkboxInput("pivot_trendlines", label = "Display Trendlines", value = TRUE)
                           )
                       ),
              fluidRow(box(width = 12,
                           plotOutput("pivotGraph"),
                           DTOutput("teamPivot")))),
      tabItem(tabName='buylows_tan',
              titlePanel('DynastyProcess Apps: Expected Points'),
              h3("Tan's Buy Lows"),
              fluidRow(box(width=12,p("A few views and filters of the database tables that I wanted to have handy and pre-filtered. Joe's building some fancy regressions app...the overachieving little bugger. -Tan")))
              ),
      tabItem(tabName='ep_about',
              titlePanel('DynastyProcess Apps: Expected Points'),
              fluidRow(column(12,
              includeMarkdown('about_ep.md')
              )))
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  track_usage(storage_mode = store_sqlite(path = "logs/"))
  
  allCols <- reactive({
    df2019 %>%
      {if (input$weeklyRadio == "Weekly") group_by(., mergename, pos, posteam, week) else group_by(., mergename, pos, posteam)} %>%
      summarise(eRushFP = sum(eRushFP, na.rm = TRUE),
                eTeamRushFP = sum(eTeamRushFP, na.rm = TRUE),
                RushFP = sum(RushFP, na.rm = TRUE),
                TeamRushFP = sum(TeamRushFP, na.rm = TRUE),
                
                RushDiff = sum(RushDiff, na.rm = TRUE),
                Rushes = sum(Rushes, na.rm = TRUE),
                RushGames = sum(RushGames, na.rm = TRUE),
                
                RushYD = sum(RushYD, na.rm = TRUE),
                
                eRecFP = sum(eRecFP, na.rm=TRUE),
                eTeamRecFP = sum(eTeamRecFP, na.rm=TRUE),
                RecFP = sum(RecFP, na.rm=TRUE),
                TeamRecFP = sum(TeamRecFP, na.rm = TRUE),
                RecDiff = sum(RecDiff, na.rm = TRUE),
                Targets = sum(Tar, na.rm = TRUE),
                TeamTargets = sum(TeamTar, na.rm = TRUE),
                
                Catches = sum(Rec, na.rm = TRUE),
                AYs = sum(AYs, na.rm = TRUE),
                TeamAYs = sum(TeamAYs, na.rm = TRUE),
                
                RecYD = sum(RecYD, na.rm = TRUE),
                aDOT = mean(AYs, na.rm = TRUE),
                RecGames = sum(RecGames, na.rm = TRUE),
                
                eTDRush = sum(eTDRush, na.rm = TRUE),
                RushTD = sum(RushTD, na.rm = TRUE),
                RushTDDiff = RushTD - eTDRush,
                
                eRushYD = sum(eRushYD, na.rm = TRUE),
                RushYD = sum(RushYD, na.rm = TRUE),
                RushYDDiff = RushYD - eRushYD,
                
                eRush1D = sum(eRush1D, na.rm = TRUE),
                Rush1D = sum(Rush1D, na.rm = TRUE),
                Rush1DDiff = Rush1D - eRush1D,
                
                eTDRec = sum(eTDRec, na.rm = TRUE),
                RecTD = sum(RecTD, na.rm = TRUE),
                RecTDDiff = RecTD - eTDRec,
                
                eRecYD = sum(eRecYD, na.rm = TRUE),
                RecYD = sum(RecYD, na.rm = TRUE),
                RecYDDiff = RecYD - eRecYD,
                
                eRec1D = sum(eRec1D, na.rm = TRUE),
                Rec1D = sum(Rec1D, na.rm = TRUE),
                Rec1DDiff = Rec1D - eRec1D,
                
                eTD = eTDRush + eTDRec,
                TDs = RushTD + RecTD,
                TDDiff = TDs - eTD,
                
                eYD = eRushYD + eRecYD,
                YDs = RushYD + RecYD,
                YDDiff = YDs - eYD,
                
                e1D = eRush1D + eRec1D,
                total1Ds = Rush1D + Rec1D,
                total1Ddiff = total1Ds - e1D,
                
                eFP = eRecFP + eRushFP,
                FP = RecFP + RushFP,
                Diff = FP - eFP,
                
                #eFP1D = eRecFP1D + eRushFP1D,
                #FP1D = RecFP1D + RushFP1D,
                #Diff1D = FP1D - eFP1D,
                
                eTeamFP = eTeamRecFP + eTeamRushFP,
                TeamFP = TeamRecFP + TeamRushFP,
                TeamDiff = TeamFP - eTeamFP,
                #eTeamFP1D = eTeamRecFP1D + eTeamRushFP1D,
                #TeamFP1D = TeamRecFP1D + TeamRushFP1D,
                #TeamDiff1D = TeamFP1D - eTeamFP1D
                AYshare = AYs / TeamAYs,
                TargetShare = Targets / TeamTargets,
                WOPR = 1.5*TargetShare + 0.7*AYshare,
                RACR = RecYD / AYs,
                YPTPA = RecYD / TeamTargets,
                eFPshare = eFP / eTeamFP,
                FPshare = FP/ TeamFP,
                Games = sum(Games, na.rm = TRUE), 
                `eFP/G` = eFP / Games,
                `FP/G` = FP / Games,
                `Diff/G` = `FP/G` - `eFP/G`) %>%
      ungroup()

  })
  
  filter1 <- reactive({
    allCols() %>%
      {if (input$selectTeam != "All") filter(., posteam == input$selectTeam) else . } %>%
      {if (input$selectPos  != "All") filter(., pos == input$selectPos) else .} %>%
      {if (input$weeklyRadio == "Weekly" & input$selectWeeks != "All") filter(., week %in% input$selectWeeks) else .} 
  })
  
  filter2 <- reactive({
    filter1() %>%
      {if (input$selectPlayers != "All") filter(., mergename %in% input$selectPlayers) else .}
  })
  
  selCols <- reactive({
    filter2() %>%
      {if (input$selectCol == "Exp Points" & input$weeklyRadio == "Weekly")
        dplyr::select(., week, mergename, posteam, pos, Games, eRecFP, RecFP, RecDiff, eRushFP, RushFP, RushDiff, eFP, FP, Diff, `eFP/G`, `FP/G`, `Diff/G`) %>% arrange(desc(`eFP/G`))
        else if (input$selectCol == "Rush Stats" & input$weeklyRadio == "Weekly")
          dplyr::select(., week, mergename, posteam, pos, Games, Rushes, eRushYD, RushYD, RushYDDiff, eTDRush, RushTD, RushTDDiff, eRush1D, Rush1D, Rush1DDiff) %>% arrange(desc(RushYD))
        else if (input$selectCol == "Rec Stats" & input$weeklyRadio == "Weekly")
          dplyr::select(., week, mergename, posteam, pos, Games, Targets, Catches, eRecYD, RecYD, RecYDDiff, eTDRec, RecTD, RecTDDiff, eRec1D, Rec1D, Rec1DDiff) %>% arrange(desc(RecYD))
        else if (input$selectCol == "Total Stats" & input$weeklyRadio == "Weekly")
          dplyr::select(., week, mergename, posteam, pos, Games, eYD, YDs, YDDiff, eTD, TDs, TDDiff, e1D, total1Ds, total1Ddiff) %>% arrange(desc(YDs))
        
        else if (input$selectCol == "Rate Stats" & input$weeklyRadio == "Weekly")
          dplyr::select(., week, mergename, posteam, pos, AYshare, TargetShare, WOPR, RACR, YPTPA, eFPshare, FPshare) %>% arrange(desc(eFPshare))
        else if (input$selectCol == "Exp Points")
          dplyr::select(., mergename, posteam, pos, Games, eRecFP, RecFP, RecDiff, eRushFP, RushFP, RushDiff, eFP, FP, Diff, `eFP/G`, `FP/G`, `Diff/G`) %>% arrange(desc(`eFP/G`))
        else if (input$selectCol == "Rush Stats")
          dplyr::select(., mergename, posteam, pos, Games, Rushes, eRushYD, RushYD, RushYDDiff, eTDRush, RushTD, RushTDDiff, eRush1D, Rush1D, Rush1DDiff) %>% arrange(desc(RushYD))
        else if (input$selectCol == "Rec Stats")
          dplyr::select(., mergename, posteam, pos, Games, Targets, Catches, eRecYD, RecYD, RecYDDiff, eTDRec, RecTD, RecTDDiff, eRec1D, Rec1D, Rec1DDiff) %>% arrange(desc(RecYD))   
        else if (input$selectCol == "Total Stats")
          dplyr::select(., mergename, posteam, pos, Games, eYD, YDs, YDDiff, eTD, TDs, TDDiff, e1D, total1Ds, total1Ddiff) %>% arrange(desc(YDs))        
        else if (input$selectCol == "Rate Stats")
          dplyr::select(., mergename, posteam, pos, Games, AYshare, TargetShare, WOPR, RACR, YPTPA, eFPshare, FPshare) %>% arrange(desc(eFPshare))
      }
    
  })
  
  weekPivot <- reactive({
    filter2() %>%
      select(mergename, week, input$selectVar) %>%
      arrange(week) %>%
      pivot_wider(names_from = week,
                  names_prefix = "Week",
                  values_from = input$selectVar) %>%
      mutate(Total = rowSums(.[2:ncol(.)], na.rm = TRUE),
             Avg = rowMeans(.[2:ncol(.)],na.rm=T)) %>% 
      arrange(desc(Avg))
    
  })
  
  observeEvent({input$selectTeam
    input$selectPos
    input$selectPlayers},{
      currentPlayer <- input$selectPlayers
      if (currentPlayer[1] == "All" & length(currentPlayer) > 1)
      {currentPlayer <- currentPlayer[!currentPlayer %in% "All"]}
      if("All" %in% currentPlayer & currentPlayer[1] != "All")
      {currentPlayer <- c("All")}
      
      updateSelectizeInput(session, 'selectPlayers',
                           choices = c("All", as.character(sort(unique(filter1()$mergename)))),
                           selected = currentPlayer
      )
    })
  
  observeEvent({input$selectWeeks},{
      currentWeeks <- input$selectWeeks
      if (currentWeeks[1] == "All" & length(currentWeeks) > 1)
      {currentWeeks <- currentWeeks[!currentWeeks %in% "All"]
      print(currentWeeks)}
      if("All" %in% currentWeeks & currentWeeks[1] != "All")
      {currentWeeks <- c("All")}
      
      updateSelectizeInput(session, 'selectWeeks',
                           choices = c("All", sort(unique(df2019$week))),
                           selected = currentWeeks
      )
    })
  
  output$teamTable <- renderDT({
    #print(colnames(filter2()))
    datatable(selCols(),
              rownames=T,
              #filter=if(input$datatable_filters){'top'} else {'none'},
              options(
                scrollX=TRUE,
                paging=TRUE,
                searching=FALSE)) %>%
      #formatRound(columns=c((ncol(df2)-15):ncol(df2)), digits=1)
      formatRound(columns=c(4:ncol(filter2())), digits = if (input$selectCol == "Rate Stats") {2} else {1})
  })
  
  observeEvent(input$maintabs=='ep_weekly',{
    updateRadioButtons(session,'weeklyRadio',selected='Weekly')
  })
  
  
  output$pivotGraph <- renderPlot({
    plotdf <- filter2()
    plotdf[is.na(plotdf)]<-NaN
#    plotdf<-plotdf #%>% 
      #filter(!is.na(input$selectVar))
    
    
    ggplot(plotdf,
           aes_string(x = plotdf$week, y = input$selectVar, color = plotdf$mergename)) +
      geom_point(size = 3) +
      theme_bw() + 
      labs(x="Week",title=paste0("Weekly Summary \n",input$selectVar,": ",input$selectTeam," | ",input$selectPos)) +
      theme(plot.title = element_text(face='bold', family = 'serif')) +
      if(input$pivot_trendlines){geom_smooth(method = "gam", fill = NA)}
  })
  
  output$teamPivot <- renderDT({
    req(input$weeklyRadio == "Weekly")
    
    weekPivot() %>% 
      datatable(
              rownames=T,
              options(
                scrollX=TRUE,
                paging=FALSE,
                searching=FALSE)) %>%
      formatRound(columns = c(2:ncol(selCols())), digits = if (grepl('share',input$selectVar)) {3} else {1})
  })
})

shinyApp(ui, server)