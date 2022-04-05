
# Source Theme
source('src/components/layout/AppTheme.R')



# Dashboard Body Module
#################################

AppBodyContentUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::uiOutput(ns('loginUI')),
    AppPagesUI(ns('AppPages'))
  )
}


AppBody <- function(input, output, session, ...) {
  
  ns <- session$ns
  rlang::env_bind(parent.env(environment()), ...)
  
  # Authentication
  if (is.null(shinyproxyauth())) {
    output$loginUI <- shiny::renderUI({
      loginUI(ns('login'))
    })
  }
  shiny::callModule(logout, "logout", session = global)
  shiny::callModule(login, "login", global = global, external_auth = shinyproxyauth())
  
  shiny::callModule(AppPages, 'AppPages', ...)
}



# Dashboard Body UI
#################################

AppBodyUI <- shinydashboard::dashboardBody(
  
  # Scripts
  shinyjs::useShinyjs(),
  AppTheme,
  htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  AppBodyContentUI('AppBodyContent')
)




