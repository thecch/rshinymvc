
abContentModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('MainUI'))
  )
}

abContentModule <- function(input, output, session, pageName, appData, ...) {
  
  ns <- session$ns
  env_bind(parent.env(environment()), ...)
  credentials <- reactive({ req(session$userData$credentials()) })
  username <- reactive({ req(credentials()$info$username) })
  observe({ appendAccessLog(username(), getwd(), session$ns('name'), '', '') })

	output$MainUI <- renderUI({
		box(width = 12, 
		  pageName
		)
	})
	
	
	# Export Data
	#####################
	dataExport <- reactiveValues(
	  'test' = 'Content'
	)
	
	appData[[pageName]] <- reactive({ reactiveValuesToList(dataExport) })
}


# Page Config
#################################

abContentPageConfig <- list(
  
  # Title for menu
  'title' = 'Content',
  
  # Icon for menu
  'icon' = 'box',
  
  # Sub-menu
  'submenu' = 'Content',
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('admin')
)








