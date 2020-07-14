suppressPackageStartupMessages({
  # Data Import
  library(arrow)

  # Data Manipulation
  library(dplyr)
  library(purrr)
  library(tidyr)

  # Shiny libs
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)

})

source("fn_ui_desktop.R")

ui <- dashboardPage(
  ui_header("SFBX Similarity Scores"),
  ui_sidebar(menuItem("Main",tabName = "main",icon = icon("quidditch",class = "white")),
             external_menuItem(text = "About",
                               href = "https://dynastyprocess.com",
                               icon = icon("rocket",class = "white"))),
  dashboardBody(
    tabItem(tabName = "main",
            box(title = "Select a Team!",
                status = "danger",
                width = 4)
            )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
