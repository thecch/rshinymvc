


# Source App Widgets
#################################

# Widget Directory
widgetDir <- 'src/components/widgets'

# Source Widgets
list.files(widgetDir, recursive = T) %>%
  magrittr::extract(. != widgetDir & str_detect(., '/')) %>%
  { paste0(widgetDir, '/', .)} %>%
  lapply(source) %>%
  invisible()



# App Widgets Override
#################################












