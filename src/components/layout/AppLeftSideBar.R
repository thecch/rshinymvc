


# Left Side Bar Module
#################################

# AppSubMenuList <- purrr::map_depth(AppPageConfigList, 1, function(x) { if ('submenu' %in% names(x)) x }) %>%
#   compact()
# 
# AppMenuList <- AppPageConfigList[which(AppPageConfigList %!in% AppSubMenuList)]

AppPageConfigList <- lapply(AppPageList, function(x) {
  get(paste0(str_extract(x, '([^/]+$)'), 'PageConfig')) %>%
    list_modify(`submenu` = ifelse(exists('submenu', .), .$submenu, x)) %>%
    list_modify(`id` = x)
})

AppMenuList <-AppPageConfigList %>%
  purrr::map(~ .$submenu) %>%
  unique()

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
  
  # output$menu <- renderUI({
  #   MenuUIList <- lapply(AppPageList, function(x) {
  #     x <- str_extract(x, '([^/]+$)')
  #     
  #     PageConfig <- get(paste0(x, 'PageConfig'))
  #     menuItem(PageConfig$title, tabName = str_to_lower(x), icon = icon(PageConfig$icon))
  #   })
  #   
  #   do.call(sidebarMenu, compact(MenuUIList))
  # })
  
  output$menu <- renderUI({
    MenuUIList <- lapply(AppMenuList, function(MenuName) {
      curAppSubMenuItemList <- AppPageConfigList %>%
        purrr::keep(~ .x$submenu == MenuName) %>%
        purrr::map(~ menuSubItem(.x$title, tabName = str_to_lower(.x$id), icon = icon(.x$icon, verify_fa = F)))
      
      if (length(curAppSubMenuItemList) > 1) {
        modify_stop_propagation(
          menuItem(MenuName, curAppSubMenuItemList, startExpanded = T)
        )
      } else {
        curAppSubMenuItemList
      }
    })

    do.call(sidebarMenu, compact(MenuUIList))
  })
}

AppLeftSideBarUI <- dashboardSidebar(
  collapsed = F,
  
  AppLeftSideBarContentUI('LeftSideBarContent')
)




