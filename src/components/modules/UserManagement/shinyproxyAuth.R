


# Shiny Proxy Extension
#################################

shinyproxyauth <- function() {
  info <- list(
    'username' = Sys.getenv("SHINYPROXY_USERNAME"),
    'permission' = stringr::str_trim(stringr::str_split(Sys.getenv("SHINYPROXY_USERGROUPS"), ',')[[1]])
  )
  info <- list('username' = 'guest', 'permissions' = stringr::str_trim(stringr::str_split('admin,guest', ',')[[1]]))
  
  if ('' %!in% info) {
    list('user_auth' = T, 'info' = info)
  } else {
    NULL
  }
}
