


# Left Side Bar Module
#################################

AppLeftSideBarContentUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    UserProfileWidgetUI(ns('ProfileBadge')),
    uiOutput(ns('menu'))
  )
}

AppLeftSideBar <- function(input, output, session, ...) {
  
  ns <- session$ns
  
  callModule(UserProfileWidget, 'ProfileBadge')
  
  output$menu <- renderUI({
    MenuUIList <- lapply(AppPageList, function(x) {
      x <- str_extract(x, '([^/]+$)')
      
      PageConfig <- get(paste0(x, 'PageConfig'))
      menuItem(PageConfig$title, tabName = str_to_lower(x), icon = icon(PageConfig$icon, verify_fa = F))
    })
    
    do.call(sidebarMenu, compact(MenuUIList))
  })
}

AppLeftSideBarUI <- dashboardSidebar(
  collapsed = F,
  
  AppLeftSideBarContentUI('LeftSideBarContent')
)


