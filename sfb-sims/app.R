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
  ui_header("SFBX Similarity Scores"),
  ui_sidebar(menuItem("Main",tabName = "main",icon = icon("quidditch",class = "white")),
             external_menuItem(text
                               = "Twitter",
                               href = "https://twitter.com/_tanho",
                               icon = icon("twitter",class = "white"))),
  dashboardBody(
    tabItem(tabName = "main",
            fluidRow(
              box(title = "About",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 4),
              box(title = "Select a Team!",
                  status = "danger",
                  width = 8,
                  pickerInput("franchise_name","Franchise",
                              choices = sfb_teams,
                              selected = sample(sfb_teams,1),
                              width = '100%',
                              options = list(
                                `live-search` = TRUE,
                                `size` = 10
                              )),
                  footer = actionButton("load_franchise",
                                        label = "Load Similarity Scores",
                                        width = '100%',
                                        class = 'btn-success')
              )),
            uiOutput('similarity_scores')
    )
  )
)

server <- function(input, output, session) {

  user <- eventReactive(input$load_franchise,input$franchise_name)

  simscores_player <- reactive( calculate_playersims(sfb_picks,user()) )

  output$dt_simscore_players <- renderDT({
    simscores_player() %>%
      datatable_playersims() })

  simscores_strategy <- reactive(calculate_strategysims(pca_dist,user()))

  output$dt_simscore_strategies <- renderDT({
    simscores_strategy() %>%
      datatable_playersims()

  })

  user_strategy <- reactive({

    calculate_strategy(pca_juice,
                       user(),
                       pca_desc)

  })

  user_strategy_summary <- reactive({


  })

  output$similarity_scores <- renderUI({

    req(user())

    bs4Dash::bs4CardLayout(type = "group",
      box(title = "Similarity Scores: Players",
          status = "danger",
          width = NULL,
          DTOutput("dt_simscore_players")),
      box(title = "Similarity Scores: Strategies",
          status = "danger",
          width = NULL,
          DTOutput("dt_simscore_strategies")
      )
    )

  })




observe(user_strategy())

}

shinyApp(ui, server)
