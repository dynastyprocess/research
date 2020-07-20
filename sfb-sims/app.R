suppressPackageStartupMessages({
  # Data Import
  library(arrow)

  # Data Manipulation
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(forcats)
  library(rlang)
  library(glue)
  library(stringr)

  # Shiny libs
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(DT)

})

## Load Data and Functions In Global ##

source("R/fn_ui.R")
source("R/fn_server.R")

sfb_picks <- read_parquet("data/sfb_picks.pdata")

sfb_teams <- unique(sfb_picks$franchise_name)

pca_factors <- read_parquet("data/pca_factors.pdata")

pca_juice <- read_parquet("data/pca_juice.pdata")

pca_dist <- read_parquet("data/pca_dist.pdata")

pca_desc <- read_parquet("data/pca_descriptions.pdata")

ui <- dashboardPage(
  sidebar_collapsed = TRUE,
  ui_header("SFBX Similarity Scores"),
  ui_sidebar(
    menuItem("Main",tabName = "main",icon = icon("quidditch",class = "white")),
    external_menuItem(text
                      = "Twitter",
                      href = "https://twitter.com/_tanho",
                      icon = icon("twitter",class = "white"))),
  dashboardBody(
    tabItem(tabName = "main",
            fluidRow(
              # fluidRow(
              # width = 4,
              box(title = "Select a Team!",
                  status = "danger",
                  width = 4,
                  pickerInput("franchise_name","Franchise",
                              choices = sfb_teams,
                              selected = sample(sfb_teams,1),
                              width = '100%',
                              options = list(
                                `live-search` = TRUE,
                                `size` = 10
                              )),
                  br(),
                  uiOutput("strategy_statement"),
                  br(),
                  footer = actionButton("load_franchise",
                                        label = "Load Similarity Scores",
                                        width = '100%',
                                        class = 'btn-success')
              ),
              br(),
              column(8,uiOutput('similarity_scores')),
            ),
            fluidRow(
              box(title = "My Team",
                  status = "danger",
                  width = 4,
                  DTOutput("my_team"))
            )


    )
  )
)


server <- function(input, output, session) {

  user <- eventReactive(input$load_franchise, input$franchise_name)

  output$my_team <- renderDT({
    get_team(sfb_picks, user()) %>%
      datatable_myteam()
  })

  simscores_player <- reactive({
    calculate_playersims(sfb_picks, user())
    })

  output$dt_simscore_players <- renderDT({
    simscores_player() %>%
      datatable_playersims()
  })

  simscores_strategy <- reactive({
    calculate_strategysims(pca_dist, user())
    })

  output$dt_simscore_strategies <- renderDT({
    simscores_strategy() %>%
      datatable_playersims()
  })

  user_strategy <- reactive({
    calculate_strategy(pca_juice,
                       user(),
                       pca_desc)
  })

  output$strategy_statement <- renderUI({
    HTML(user_strategy())
    })

  output$similarity_scores <- renderUI({

    req(user())

    bs4CardLayout(
      type = "deck",
      box(
        title = "Similarity Scores: Strategies",
        status = "danger",
        width = NULL,
        DTOutput("dt_simscore_strategies")
      ),
      box(
        title = "Similarity Scores: Players",
        status = "danger",
        width = NULL,
        DTOutput("dt_simscore_players")
      )
    )
  })

  # observe(user_strategy())

}

shinyApp(ui, server)
