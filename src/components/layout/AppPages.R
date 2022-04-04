


# Dashboard Pages Module
#################################

AppPageList <- str_extract(list.files(paste0('src/pages/'), recursive = T), '[^.]+') %>%
  purrr::discard(function(x) isTRUE(tryCatch(get(paste0(x, 'PageConfig'))$disabled)))

AppPagesUI <- function(id) {
  
  ns <- shiny::NS(id)

  PageUIList <- lapply(AppPageList, function(x) {
    shinydashboard::tabItem(tabName = stringr::str_to_lower(x), get(paste0(x, 'ModuleUI'))(ns(paste0(x, 'PageModule'))))
  })
  
  do.call(tabItems, compact(PageUIList))
}

AppPages <- function(input, output, session, ...) {

  ns <- session$ns
  
  permissions <- reactive({
    req(session$userData$credentials()$info$permissions)
  })
  
  AppPagesData <- do.call(shiny::reactiveValues, sapply(AppPageList, function(x) NULL))
  
  observeEvent(permissions(), {
    AppPageList %>%
      purrr::keep(~ any(permissions() %in% get(paste0(.x, 'PageConfig'))[['permission']])) %>%
      lapply(function(x) {
        shiny::callModule(get(paste0(x, 'Module')), paste0(x, 'PageModule'), pageName = x, appData = AppPagesData, ...)
      })
  }, once = T)

}












