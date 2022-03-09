


# User Profile Widget
#################################

UserProfileWidgetUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('ProfileBadge'))
  )
}

UserProfileWidget <- function(input, output, session, ...) {
  
  ns <- session$ns
  
  output$ProfileBadge <- renderUI({

    sidebarUserPanel(
      "Demo",
      image = "img/usr/user_profile.png"
    )
    
  })
}









