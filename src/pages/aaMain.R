
aaMainModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('MainUI'))
  )
}

aaMainModule <- function(input, output, session, pageName, appData, ...) {
  
  ns <- session$ns
  env_bind(parent.env(environment()), ...)
  credentials <- reactive({ req(session$userData$credentials()) })
  username <- reactive({ req(credentials()$info$username) })
  observe({ appendAccessLog(username(), getwd(), pageName, '', '') })

	output$MainUI <- renderUI({
	  # req(appData[['abContent']]())
	  
		column(12,
		  HTML("<font size=5 color='blue'>Hi,<br/>Welcome to Shiny Template.</font>"),
		  br()
		)
	})
	
	
	# Export Data
	#####################
	
	dataExport <- reactiveValues(
	  'test' = 'Main'
	)
	
	appData[[pageName]] <- reactive({ reactiveValuesToList(dataExport) })
}


# Page Config
#################################

aaMainPageConfig <- list(
  
  # Title for menu
  'title' = 'Main',
  
  # Icon for menu
  'icon' = 'home',
  
  # Sub-menu
  'submenu' = 'Main',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('admin', 'guest')
)








