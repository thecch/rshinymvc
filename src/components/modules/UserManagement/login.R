


# Log In Module
#################################

returnClickJS <- '
$(document).keyup(function(event) {
  if ($("#login-password").is(":focus") && (event.keyCode == 13)) {
    $("#login-button").click();
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
      shiny::tags$script(shiny::HTML(returnClickJS)),
      shiny::tags$script(type = "text/javascript",
        src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js"
      )
    ),
    shiny::wellPanel(
      shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),
      shiny::textInput(ns("user_name"), shiny::tagList(shiny::icon("user"), user_title), value = default_username),
      shiny::passwordInput(ns("password"), shiny::tagList(shiny::icon("unlock-alt"), pass_title), value = default_password),
      shiny::div(style = "text-align: center;", shiny::actionButton(ns("button"), login_title, class = "btn-primary")),
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
      session$reload()
    })
    
    # Login Button Listener
    shiny::observeEvent(input$button, {
                  
      # check for match of input username to username column in data
      row_username <- data %>%
        dplyr::filter(username == input$user_name) %>%
        dplyr::pull(username)
      
      if (length(row_username)) {
        password_match <- data %>%
          dplyr::filter(username == input$user_name) %>%
          dplyr::pull(password_hash) %>%
          sodium::password_verify(input$password)
      } else {
        password_match <- F
      }
      
      # # if user name row and password name row are same, credentials are valid
      if (length(row_username) == 1 && password_match) {
        credentials$user_auth <- T
        credentials$info <- data %>%
          dplyr::filter(username == input$user_name) %>%
          dplyr::collect()
      } else {
        # if not valid temporarily show error message to user
        shinyjs::toggle(id = "error", anim = T, time = 1, animType = "fade")
        shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = T, time = 1, animType = "fade"))
      }
    })
  }
  
  session$userData$credentials <- shiny::reactive({ credentials })
}



# Log Out Module
#################################

logoutUI <- function(id, label = "Log out", class = "btn-danger") {
  ns <- shiny::NS(id)
  
  shinyjs::hidden(
    shiny::actionButton(ns("button"), label, class = class)
  )
}

logout <- function(input, output, session) {
  session$userData$logout <- shiny::reactive({ input$button })
}





