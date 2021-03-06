


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
      name = req(session$userData$credentials()$info$name),
      subtitle = req(session$userData$credentials()$info$username),
      image = "img/usr/user_profile.png"
    )
    
  })
}



# Widget Config
#################################








