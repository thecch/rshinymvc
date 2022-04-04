


zbWidgetAccessLogModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    column(12, inputPanel(actionButton(ns('refresh'), 'Refresh Data'))),
    box(width = 12, status = "primary", collapsible = T,
      column(4, plotlyOutput(ns('userView'))),
      column(4, plotlyOutput(ns('userDateView'))),
      column(4, plotlyOutput(ns('dateUserView')))
    ),
    box(title = 'accessUserView', width = 12, status = "primary", collapsible = T, DTOutput(ns('accessUser'))),
    box(title = 'accessLogDetails', width = 12, status = "primary", collapsible = T, DTOutput(ns('accessLog')))
  )
}

zbWidgetAccessLogModule <- function(input, output, session, ...) {
  
  ns <- session$ns
  env_bind(parent.env(environment()), ...)
  credentials <- reactive({ req(session$userData$credentials()) })
  observe({ appendAccessLog(req(credentials()$info$username), getwd(), session$ns('name'), '', '') })
  
  # Access Log Data
  accessLog <- reactive({
    input$refresh
    
    getAccessLog()
  })
  
  # Access Log Table
  output$accessLog <- renderDT({
    accessLog() %>%
      formatDTDisplay()
  })
  
  # Access Log Bar Charts
  output$userView <- renderPlotly({
    accessLog() %>%
      group_by(`username`) %>%
      summarise(`Date` = n()) %>%
      plot_ly(x = ~username, y = ~Date, type = 'bar') %>%
      layout(title = 'Access count by user', xaxis = list(title = 'Username'))
  })

  output$userDateView <- renderPlotly({
    accessLog() %>%
      group_by(`username`, `Date`) %>%
      summarise(`Module` = n(), .groups = 'drop_last') %>%
      summarise(`Date` = n()) %>%
      plot_ly(x = ~username, y = ~Date, type = 'bar') %>%
      layout(title = 'Access count by user (unique Date)', xaxis = list(title = 'Username'), yaxis = list(title = 'Visits'))
  })


  output$dateUserView <- renderPlotly({
    accessLog() %>%
      group_by(`Date`, `username`) %>%
      summarise(`Module` = n(), .groups = 'drop_last') %>%
      summarise(`username` = n()) %>%
      plot_ly(x = ~username, y = ~Date, type = 'bar') %>%
      layout(title = 'Access count by Date (Unique Users)', xaxis = list(title = 'Unique Users'))
  })
  
  # User View
  output$accessUser <- renderDT({
    accessLog() %>%
      group_by(`username`) %>%
      summarise(AccessCnt = n(), uniqueDateCnt = length(unique(Date)), earliestDate = min(`Date`), latestDate = max(`Date`)) %>%
      ungroup() %>%
      formatDTDisplay()
  })
}



# Page Config
#################################

zbWidgetAccessLogPageConfig <- list(
  
  # Disable Page
  # disabled = T,
  
  # Title for menu
  'title' = 'Access Log',
  
  # Icon for menu
  'icon' = 'users',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('admin', 'guest')
)






