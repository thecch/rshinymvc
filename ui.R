
source('src/global.R')

# UI
################################

shinyUI(
  if (packageVersion('shinydashboardPlus') < 1)
    shinydashboardPlus::dashboardPagePlus(
      header = AppHeaderUI,
      sidebar = AppLeftSideBarUI,
      body = AppBodyUI
    )
  else 
    shinydashboardPlus::dashboardPage(
      AppHeaderUI,
      AppLeftSideBarUI,
      AppBodyUI
    )
)

# End of script