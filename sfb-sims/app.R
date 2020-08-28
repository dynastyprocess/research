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
  library(ggplot2)
  library(plotly)
  library(gfonts)

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

options(dplyr.summarise.inform = FALSE)

ui <- dashboardPage(
  title = "SFBX Similarity Scores",
  sidebar_collapsed = TRUE,
  ui_header("SFBX Similarity Scores"),
  ui_sidebar(
    menuItem("Main",tabName = "main",icon = icon("quidditch",class = "white")),
    external_menuItem(text
                      = "Twitter",
                      href = "https://twitter.com/_tanho",
                      icon = icon("twitter",class = "white"))),
  dashboardBody(
    use_font("roboto", "www/css/fira-sans.css"),
    tabItem(tabName = "main",
            fluidRow(
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
                  # br(),
                  actionButton("load_franchise",
                               label = "Load Similarity Scores",
                               width = '100%',
                               class = 'btn-success'),
                  br(),
                  br(),
                  uiOutput("strategy_statement"),
                  br(),
                  uiOutput("comparison_picker")
              ),
              br(),
              column(8,uiOutput('similarity_scores')),
            ),
              uiOutput("pca_plot"),
            br(),
            fluidRow(uiOutput("team_tables"))
    )
  )
)


server <- function(input, output, session) {


  # w <- Waiter$new(id = c("strategy_statement", "comparison_picker","pca_plot","team_tables","similarity_scores"))

  user <- eventReactive(input$load_franchise, {
    # w$show()
    input$franchise_name
    })

  comparisons <- reactive({
    req(input$comparison_1,input$comparison_2)

    unique(c(input$comparison_1,input$comparison_2))

  })

  output$dt_teams <- renderDT({
    get_teamrosters(sfb_picks, user(), comparisons()) %>%
      datatable_myteam(user(),comparisons())
  })

  output$team_tables <- renderUI({

    req(user(),comparisons())

    box(title = "Roster Comparisons",
        status = "danger",
        maximizable = TRUE,
        width = 12,
        DTOutput("dt_teams"))
  })

  output$comparison_picker <- renderUI({

    req(simscores_player(),simscores_strategy())

    generate_comparisoninputs(simscores_strategy(),simscores_player())

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

  pca_data <- reactive({

    calculate_pcatable(pca_juice,pca_desc,user(),comparisons())

  })

  output$pca_plotly <- renderPlotly({

    generate_pcachart(pca_data())

  })

  output$pca_plot <- renderUI({

    req(user())

    fluidRow(
    box(title = "Strategy Comparison",
        status = "danger",
        maximizable = TRUE,
        width = 12,

        "This chart shows you where each of the teams in focus fall on each strategic axis. For more on this, see the article at {in-progress}.",
        br(),

        plotlyOutput('pca_plotly')
        ))
  })

  output$strategy_statement <- renderUI({

    req(user())

    HTML(user_strategy())
    })

  output$similarity_scores <- renderUI({

    req(user())

    bs4CardLayout(
      type = "group",
      box(
        title = "Similarity Scores: Strategies",
        status = "danger",
        maximizable = TRUE,
        width = NULL,
        DTOutput("dt_simscore_strategies")
      ),
      box(
        title = "Similarity Scores: Players",
        status = "danger",
        maximizable = TRUE,
        width = NULL,
        DTOutput("dt_simscore_players")
      )
    )
  })


}

shinyApp(ui, server)
