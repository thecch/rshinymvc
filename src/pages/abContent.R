
abContentModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('MainUI'))
  )
}

abContentModule <- function(input, output, session, pageName, appData, ...) {
  
	ns <- session$ns

	output$MainUI <- renderUI({
		box(
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
  'submenu' = 'Content'
)








