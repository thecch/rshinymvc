
aaMainModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('MainUI'))
  )
}

aaMainModule <- function(input, output, session, pageName, appData, ...) {
  
	ns <- session$ns

	output$MainUI <- renderUI({
	  # req(appData[['abContent']]())
	  
		HTML(paste0(
		  "<font size=5 color='blue'>Hi,<br/>Welcome to Lipidall Shiny Template. 1</font>",
		  appData[['abContent']]()$test
		))
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
  'submenu' = 'Main'
)








