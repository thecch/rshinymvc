


# Left Side Bar Menu
#################################

AppPageConfigList <- lapply(AppPageList, function(x) {
  get(paste0(str_extract(x, '([^/]+$)'), 'PageConfig')) %>%
    list_modify(`submenu` = ifelse(exists('submenu', .), .$submenu, x)) %>%
    list_modify(`id` = x)
})

AppMenuList <- AppPageConfigList %>%
  purrr::map(~ .$submenu) %>%
  unique()



# Left Side Bar Module
#################################

AppLeftSideBarContentUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    UserProfileWidgetUI(ns('UserProfileWidget')),
    shinydashboard::sidebarMenuOutput(ns('menu'))
  )
}

AppLeftSideBar <- function(input, output, session, ...) {
  
  ns <- session$ns
  permissions <- reactive({
    req(session$userData$credentials()$info$permissions)
  })
  
  observeEvent(permissions(), {
    callModule(UserProfileWidget, 'UserProfileWidget')
    
    output$menu <- shinydashboard::renderMenu({
      MenuUIList <- lapply(AppMenuList, function(MenuName) {
        curAppSubMenuItemList <- AppPageConfigList %>%
          purrr::keep(~ any(.x$permission == permissions())) %>%
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
      
      do.call(sidebarMenu, list(compact(MenuUIList), 'tabName' = MenuUIList[1]))
    })
  }, once = T)
}



# Left Side Bar UI
#################################

AppLeftSideBarUI <- dashboardSidebar(
  collapsed = F,
  
  AppLeftSideBarContentUI('LeftSideBarContent')
)



