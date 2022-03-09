
# Source Theme
source('src/components/layout/AppTheme.R')


# Dashboard Body UI
#################################

widgetUI <- fluidRow(AppPagesUI('AppPages'))

AppBodyUI <- dashboardBody(
  
  # Scripts
  shinyjs::useShinyjs(),
  AppTheme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  widgetUI
)




