
source('src/global.R')

# UI
################################

shinyUI(
  shinydashboardPlus::dashboardPage(
    AppHeaderUI,
    AppLeftSideBarUI,
    AppBodyUI
  )
)

# End of script