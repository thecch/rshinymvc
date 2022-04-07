


# Log In Module
#################################

returnClickJS <- '
$(document).keyup(function(event) {
  if ($("#%s").is(":focus") && (event.keyCode == 13)) {
    $("#%s").click();
  }
});
'

loginUI <- function(
  id, title = "Please Log In", user_title = "User Name", pass_title = "Password", default_username = 'guest',
  login_title = "Log in", error_message = "Invalid username or password!", default_password = 'guest'
) {
  
  ns <- shiny::NS(id)
  
  shiny::div(id = ns("panel"), style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(sprintf(returnClickJS, ns("password"), ns("button")))),
      shiny::tags$script(type = "text/javascript",
        src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js"
      )
    ),
    shiny::wellPanel(
      shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),
      shiny::textInput(ns("user_name"), shiny::tagList(shiny::icon("user"), user_title), value = default_username),
      shiny::passwordInput(ns("login_password"), shiny::tagList(shiny::icon("unlock-alt"), pass_title), value = default_password),
      shiny::div(style = "text-align: center;", shiny::actionButton(ns("button"), login_title)),
      shinyjs::hidden(
        shiny::div(id = ns("error"), style = "color: red; font-weight: bold; padding-top: 5px;", class = "text-center",
          shiny::tags$p(error_message)
        )
      )
    )
  )
}

login <- function(input, output, session, data = getUserBase(), external_auth = NULL, ...) {
  
  if (!is.null(external_auth)) {
    credentials <- shiny::reactiveValues(user_auth = external_auth$user_auth, info = external_auth$info)
  } else {
    credentials <- shiny::reactiveValues(user_auth = F, info = NULL)
    
    shiny::observeEvent(credentials$user_auth, {
      if (shiny::isTruthy(credentials$user_auth)) {
        shinyjs::runjs('$("body").removeClass("sidebar-collapse");')
        shinyjs::hide(id = "panel")
        shinyjs::show(selector = '#logout-button')
      } else {
        shinyjs::runjs('$("body").addClass("sidebar-collapse");')
        shinyjs::show(id = "panel")
        shinyjs::hide(selector = '#logout-button')
      }
    }, ignoreInit = F)
    
    # Reload Session on logout to remove all data
    shiny::observeEvent(session$userData$logout(), {
      credentials$user_auth <- F
      credentials$info <- NULL
      session$reload()
    })
    
    # Login Button Listener
    shiny::observeEvent(input$button, {
                  
      credentials$user_auth <- verify_user(input$user_name, input$login_password)
      
      # # if user name row and password name row are same, credentials are valid
      if (credentials$user_auth) {
        credentials$info <- data %>%
          dplyr::select(`name`, `username`, `permissions`, `email`) %>%
          dplyr::filter(`username` == input$user_name) %>%
          dplyr::collect() %>%
          unlist() %>%
          as.list() %>%
          purrr::map_at('permissions', ~ stringr::str_split(.x, ',')[[1]])
      } else {
        # if not valid temporarily show error message to user
        shinyjs::toggle(id = "error", anim = T, time = 1, animType = "fade")
        shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = T, time = 1, animType = "fade"))
      }
    })
  }
  
  session$userData$credentials <- shiny::reactive({ credentials })
}

# Verify Login Function
verify_user <- function(verify_username, verify_password, data = getUserBase()) {
  # check for match of input username to username column in data
  row_username <- data %>%
    dplyr::filter(`username` == verify_username) %>%
    dplyr::pull(`username`)
  
  if (shiny::isTruthy(verify_username) && shiny::isTruthy(verify_password) && shiny::isTruthy(length(row_username))) {
    data %>%
      dplyr::filter(`username` == verify_username) %>%
      dplyr::pull(password_hash) %>%
      sodium::password_verify(verify_password)
  } else {
    F
  }
}


# Log Out Module
#################################

logoutUI <- function(id, label = "Log out") {
  ns <- shiny::NS(id)
  
  shinyjs::hidden(
    shiny::actionButton(ns("button"), label)
  )
}

logout <- function(input, output, session) {
  session$userData$logout <- shiny::reactive({ input$button })
}





