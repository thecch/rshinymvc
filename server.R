
shinyServer(function(input, output, session) {
  
  # Source global variables
  source('src/global.R')
  
  # App Layout
  ###########################

  callModule(AppLeftSideBar, 'LeftSideBarContent')
  callModule(AppPages, 'AppPages')
  
  
  # App Data
  ###########################
  
  
})

# End of script