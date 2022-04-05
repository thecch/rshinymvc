


# Header UI
#################################

widgetUI <- tags$li(class = "dropdown",
  logoutUI('logout')
)

AppHeaderUI <- if (packageVersion('shinydashboardPlus') < 1) {
  shinydashboardPlus::dashboardHeaderPlus(
    
    # Branding
    title = tagList(
      tags$a(href = '#top', class = "logo-lg main-logo", img(src = 'img/logo/logo.jpg')),
      tags$a(href = '#top', class = "main-logo", img(src = 'img/logo/favicon.png'))
    ),
    
    # Logout
    widgetUI,
    
    # Right Sidebar
    rightSidebarIcon = "bars"
  )
} else {
  shinydashboardPlus::dashboardHeader(
    
    # Branding
    title = tagList(
      tags$a(href = '#top', class = "logo-lg main-logo", img(src = 'img/logo/logo.jpg')),
      tags$a(href = '#top', class = "main-logo", img(src = 'img/logo/favicon.png'))
    ),
    
    # Logout
    widgetUI,
    
    # Right Sidebar
    controlbarIcon = "bars"
  )
}


