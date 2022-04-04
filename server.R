
shinyServer(function(input, output, session) {
  
  # App Layout
  #################################

  #Layout
  callModule(AppLeftSideBar, 'LeftSideBarContent')
  callModule(AppBody, 'AppBodyContent', global = session)
  
  
  
  # App Dynamic Data
  #################################
  
  

})

# End of script