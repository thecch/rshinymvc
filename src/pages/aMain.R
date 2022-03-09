
aMainModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(width = 12, status = "primary",
      uiOutput(ns('MainUI'))
    )
  )
}

aMainModule <- function(input, output, session, ...) {
  
	ns <- session$ns

	output$MainUI <- renderUI({
		HTML(sprintf("<font size=5 color='blue'>Hi,<br/>Welcome to Lipidall Shiny Template. 1</font>"))
	})
}


# Page Config
#################################

aMainPageConfig <- list(
  
  # Title for menu
  'title' = 'Main',
  
  # Icon for menu
  'icon' = 'dashboard'
)








