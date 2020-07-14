# UI functions
#
# Because sheesh those UI elements are verbose.

ui_header <- function(title,...){
  bs4Dash::dashboardHeader(skin = 'dark',
                  fixed = TRUE,
                  border = TRUE,
                  shiny::span(title,style= 'font-size:1.5em;color:#ffffff'),
                  ...)
}

ui_sidebar <- function(...){
  bs4Dash::dashboardSidebar(title = 'DynastyProcess.com',
                 fixed = TRUE,
                 skin = "dark",
                 elevation = 3,
                 opacity = 0.8,
                 url = "https://dynastyprocess.com",
                 expand_on_hover = TRUE,
                 src = "https://avatars2.githubusercontent.com/u/63691873?s=400&u=d9289a2540799f19ca6d8ad866e219ee2d602ba9&v=4",
                 bs4Dash::sidebarMenu(...))
}

# iconwrap_slider <- function(inputid,label,min,max,value,...,icon_left,icon_right){
#   shiny::fluidRow(
#     bs4Dash::column(2,shiny::icon(icon_left), style = 'padding-top:15px;'),
#     bs4Dash::column(8,shiny::sliderInput(inputId = inputid,label = label,min = min, max = max, value = value, ...)),
#     bs4Dash::column(2,shiny::icon(icon_right),style = 'padding-top:15px;text-align:right;')
#   )
# }
#

external_menuItem <- function(text = NULL, href = NULL, icon = NULL){
  tags$li(tags$a(span(icon,style = "font-size:1.1rem;"),
                 p(text,style = "padding-left: .7rem;"),
                 class = "nav-link", href = href),class = "nav-item")
}
