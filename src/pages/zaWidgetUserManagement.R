


zaWidgetUserManagementModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(width = 12,  status = "primary",
      tabsetPanel(
        tabPanel('UserList',
          shiny::tags$script(shiny::HTML(paste0(
            "function get_id(clicked_id) {",
              "Shiny.setInputValue('", ns("current_id"), "', clicked_id, {priority: 'event'});",
            "}"
          ))),
          actionButton(ns('refreshUser'), 'Refresh'),
          DTOutput(ns('user_base_ui'))
        )
        # tabPanel('Change',
        #   uiOutput(ns('ChangeForm')),
        #   uiOutput(ns('infoChangeMsg')),
        #   uiOutput(ns('uTypeChangeMsg')),
        #   uiOutput(ns('addUMsg')),
        #   uiOutput(ns('delUMsg')),
        #   uiOutput(ns('passordChangeMsg')),
        #   uiOutput(ns('ChangeFormDetails'))
        # )
      )
    )
  )
}

zaWidgetUserManagementModule <- function(input,  output,  session,  ...) {

  
  # Module Data
  ###############################
  
  # init
  ns <- session$ns
  rlang::env_bind(parent.env(environment()), ...)
  credentials <- shiny::reactive({ shiny::req(session$userData$credentials()) })
  username <- shiny::reactive({ shiny::req(credentials()$info$username) })
  shiny::observe({ appendAccessLog(username(), getwd(), session$ns('name'), '', '') })
  
  
  # Page Data
  ###############################
  rv <- reactiveValues(commit = NULL)
  
  user_base <- shiny::reactive({
    input$refreshUser
    rv$commit
    user_name <- shiny::req(username())
    permissions <- shiny::req(credentials()$info$permissions)
    
    getUserBase() %>%
      dplyr::bind_rows(rlang::set_names(rep('', ncol(.)), colnames(.))) %>% {
        if ('admin' %!in%  permissions) {
          dplyr::filter(., `username` == user_name) %>%
            dplyr::mutate('Update' = create_btns(`username`, ns, user_name, T))
        } else {
          dplyr::mutate(., 'Update' = create_btns(`username`, ns, user_name, F))
        }
      }
      
  })
  
  output$user_base_ui <- DT::renderDT({
    user_base() %>%
      dplyr::select(-`password_hash`)
  }, selection = 'single', escape = F, rownames = F, editable = F, options = list(processing = F, scrollX = T))
  
  
  # Page
  ###############################
  
  # Edit and Delete User
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit|delete|new|reset"))
    selected_username <- stringr::str_replace(input$current_id, '.*(edit_|delete_|new_|reset_)', '')
    mode <- stringr::str_extract(input$current_id, '(delete|edit|new|reset)')
    
    shiny::showModal(
        shiny::modalDialog(fade = F, 
          title = h3(sprintf('%s User', stringr::str_to_title(mode))),
          htmltools::tags$head(htmltools::tags$script(shiny::HTML(sprintf(returnClickJS, ns("confirmPassword"), ns("confirmButton"))))),
          shiny::div(style = 'padding: 16px 0 24px 0;',
            if (mode == 'delete') {
              shiny::HTML(sprintf('Are you sure you want to delete the user <b>%s</b>?', selected_username))
            } else if (mode == 'edit') {
              user_base() %>%
                filter(`username` == selected_username) %>%
                dplyr::select(-`Update`, -`password_hash`, -`username`) %>%
                tidyr::gather() %>%
                purrr::transpose() %>%
                purrr::map(~ shiny::textInput(
                  inputId = ns(.$key), label = stringr::str_to_title(stringr::str_replace(.$key, '_', ' ')), value = .$value)
                ) %>%
                { do.call('tagList', purrr::compact(.)) } %>%
                htmltools::tagInsertChildren(after = 0, (shinyjs::disabled(shiny::textInput(ns('username'), 'Username', value = selected_username))))
            } else if (mode == 'new') {
              names(user_base()) %>%
                purrr::discard(~ str_detect(.x, '(password_hash|Update)')) %>%
                unique() %>%
                purrr::map(~ shiny::textInput(inputId = ns(.), label = stringr::str_to_title(.), value = '')) %>%
                { do.call('tagList', purrr::compact(.)) } %>%
                shiny::tagAppendChildren(
                  shiny::passwordInput(ns('password'), 'New Password'),
                  shiny::passwordInput(ns('confirm_password'), 'Confirm New Password')
                )
            } else if (mode == 'reset') {
              shiny::tagList(
                shinyjs::disabled(shiny::textInput(ns('username'), 'Username', value = selected_username)),
                shiny::passwordInput(ns('password'), 'New Password'),
                shiny::passwordInput(ns('confirm_password'), 'Confirm New Password')
              )
            }
          ),
          shiny::passwordInput(ns('confirmPassword'), NULL, placeholder = 'Password', width = '100%'),
          shiny::helpText('Admin actions require password reconfirmation.', style = 'padding-bottom: 12px;'),
          shiny::fluidRow(style = "font-weight: bold;", shiny::column(12, style = 'min-height: 16 px;',
            shinyjs::hidden(shiny::p(id = ns("error"), style = 'color: #dc3545;', textOutput(ns('error_message')))),
            shinyjs::hidden(shiny::p(id = ns("success"), style = 'color: #28a745;', textOutput(ns('success_message'))))
          )),
          footer = shiny::div(
            shiny::actionButton(ns('confirmButton'), 'Confirm', class = ifelse(mode == 'delete', 'btn-danger', 'btn-info')),
            shiny::modalButton("Cancel")
          )
        )
    )
  })
  

  
  shiny::observeEvent(input$confirmButton, {
    mode <- stringr::str_extract(input$current_id, '(delete|edit|new|reset)')
    message <- list('status' = NULL, 'text' = '')
    
    if (verify_user(username(), input$confirmPassword)) {
      if (mode == 'delete') {
        deleteUser(stringr::str_replace(input$current_id, '.*(edit_|delete_|new_)', ''))
      } else if (mode == 'edit') {
        args <- names(getUserBase()) %>%
          purrr::discard(~ str_detect(.x, 'password')) %>%
          unique() %>%
          { rlang::set_names(purrr::map(., ~ input[[.]]), .) }
        
        do.call('update_info', args)
        message <- list('status' = T, 'text' = 'Info Successfully Updated')
      } else if (mode == 'new') {
        args <- c(names(getUserBase()), 'password', 'confirm_password') %>%
          purrr::discard(~ stringr::str_detect(.x, 'password_hash')) %>%
          unique() %>%
          { rlang::set_names(purrr::map(., ~ input[[.]]), .) }
        
        if (args$password == args$confirm_password) {
          do.call('add_user', args)
          message <- list(status = T, text = 'User successfully Added')
        } else {
          message <- list(status = F, text = 'New passwords does not match')
        }
      } else if (mode == 'reset') {
        args <- c('username', 'password', 'confirm_password') %>%
          { rlang::set_names(purrr::map(., ~ input[[.]]), .) }
        
        if (args$password == args$confirm_password) {
          do.call('update_info', args)
          message <- list(status = T, text = 'Password successfully reset')
        } else {
          message <- list(status = F, text = 'New passwords does not match')
        }
      }
    } else {
      message <- list(status = F, text = 'Password is incorrect')
    }
    
    if (!is.null(message$status) & message$status) {
      shinyjs::hide(id = "error", anim = T, time = 1, animType = "fade")
      shinyjs::show(id = "success", anim = T, time = 1, animType = "fade")
      shinyjs::delay(5000, shinyjs::hide(id = "success", anim = T, time = 1, animType = "fade"))
      shiny::updateTextInput(session = session, 'confirmPassword', label = NULL, value = '')
      shiny::removeModal()
      rv$commit <- input$confirmButton
      
      output$success_message <- shiny::renderText({
        message$text
      })
    } else if (!is.null(message$status) & !message$status) {
      shinyjs::hide(id = "success", anim = T, time = 1, animType = "fade")
      shinyjs::show(id = "error", anim = T, time = 1, animType = "fade")
      shinyjs::delay(5000, shinyjs::hide(id = "error", anim = T, time = 1, animType = "fade"))
      
      output$error_message <- shiny::renderText({
        message$text
      })
    }
  })
  
  
  output$ChangeForm <- renderUI({
    user_base <- user_base()
    username <- req(credentials()$info$username)
    
    inputPanel(
      selectInput(
        ns('usernameSelect'), 'Select User',
        ifelse(credentials()$info$permissions == 'admin', pull(user_base, username), username),
        username, selectize = F
      )
    )
  })
  
  output$ChangeFormDetails <- renderUI({
    selected_username <- req(input$usernameSelect)
    
    user_base <- user_base() %>%
      dplyr::filter(`username` == selected_username)
    uList <- setdiff(user_base$username, credentials()$info$username)
    
    ChangeFormList <- list(
      tabPanel('infoChange', inputPanel(
        lapply(setdiff(colnames(user_base), c('username', 'password_hash', 'permissions')), function(x) {
          textInput(ns(sprintf('%sTxt', x)), x, user_base[, x])
        }),
        actionButton(ns('infoChangeBtn'), 'Submit')
      )),
      tabPanel('password change', inputPanel(
        textInput(ns('nameTxt'), 'Name', user_base$name),
        passwordInput(ns('oldpassword'), 'Old Password'),
        passwordInput(ns('password1'), 'New Password'),
        passwordInput(ns('password2'), 'Confirm New Password'),
        actionButton(ns('pwchangeBtn'), 'Submit')
      )),
      tabPanel('UserType change', inputPanel(
        selectInput(ns('uTypeSelect'), 'Permissions', c('admin', 'user'), user_base$permissions, selectize = F),
        passwordInput(ns('oldpassword1'), 'Admin Password'),
        actionButton(ns('uTypeChangeBtn'), 'Submit')
      )),
      tabPanel('Add User', inputPanel(
        textInput(ns('addusername'), 'Username'),
        passwordInput(ns('addpassword'), 'Password'),
        passwordInput(ns('addpassword1'), 'Confirm Password'),
        passwordInput(ns('addAdminPw'), 'Admin password'),
        selectInput(ns('addpermissions'), 'Permissions', c('admin', 'user'), 'user', selectize = F),
        lapply(setdiff(colnames(user_base), c('username', 'password_hash', 'permissions')), function(x) {
          textInput(ns(sprintf('add%s', x)), x, '')
        }),
        actionButton(ns('uAddBtn'), 'Submit'),
        uiOutput(ns('addUI'))
      )),
      tabPanel('Delete User', inputPanel(
        selectInput(ns('delUsername'), 'Delete username:', uList, uList[1], selectize = F),
        passwordInput(ns('delAdminPw'), 'Requestor Password (Adminstrator)'),
        actionButton(ns('uDelBtn'), 'Submit')
      ))
    )
    
    do.call('tabsetPanel', ChangeFormList)
  })
  
  observeEvent(input$uAddBtn, {
    user_base <- user_base()
    addAdminPw <- input$addAdminPw
    addusername <- input$addusername
    addpassword <- input$addpassword
    addpassword1 <- input$addpassword1
    addpermissions <- input$addpermissions
    
    reqInfo <- user_base[which(user_base$username == credentials()$info$permissions), ]
    if (credentials()$info$permissions == 'admin') {
      if (!sodium::password_verify(reqInfo$password_hash, addAdminPw)) {
        if (addpassword != addpassword1) {
          msg <- sprintf("%s: Password and Confirm Password different", Sys.time())
        } else {
          if (addusername %in% user_base$username) {
            msg <- sprintf("%s: username already exists", Sys.time())
          } else {
            addList <- list()
            for (x in setdiff(colnames(user_base), c('username', 'password_hash', 'permissions'))) {
              addList[[x]] <- input[[sprintf("add%s", x)]]
            }
            addUser2(addusername, addpassword, addpermissions, addList, userListFile = userListFile)
            msg <- sprintf("%s: new user %s added", Sys.time(), addusername)
          }
        }
      } else {
        msg <- sprintf("%s: Requestor password incorrect", Sys.time())
      }
    } else {
      msg <- sprintf("%s: Requestor does not have admin rights", Sys.time())
    }
    
    output$addUMsg <- renderUI({
      HTML(msg)
    })
  })
  
  observeEvent(input$uTypeChangeBtn, {
    output$uTypeChangeMsg <- renderUI({
      user_base <- user_base()
      usernameSelect <- input$usernameSelect
      oldpassword1 <- input$oldpassword1
      uTypeSelect <- input$uTypeSelect
      userDbInfo <- user_base[which(user_base$username == usernameSelect), ]
      
      reqInfo <- user_base[which(user_base$username == credentials()$info$username), ]
      if (reqInfo$permissions == 'admin') {
        if (!sodium::password_verify(reqInfo$password_hash, oldpassword1)) {
          HTML(sprintf("%s: Wrong password provided", Sys.time()))
        } else {
          gChangePermission(uTypeSelect, usernameSelect, userListFile = userListFile)
          HTML(sprintf("%s: Rights changed for %s to %s", Sys.time(), usernameSelect, uTypeSelect))
        }
      } else {
        HTML(sprintf("%s: You have no admin rights", Sys.time()))
      }
    })
  })
  
  observeEvent(input$pwchangeBtn, {
    output$passordChangeMsg <- renderUI({
      user_base <- user_base()
      oldpassword <- input$oldpassword
      usernameSelect <- input$usernameSelect
      password1 <- input$password1
      password2 <- input$password2
      userDbInfo <- user_base[which(user_base$username == usernameSelect), ]
      
      if (!sodium::password_verify(userDbInfo$password_hash, oldpassword)) {
        HTML(sprintf('%s: Wrong old password', Sys.time()))
      } else if (password1 == password2) {
        gChangePassword(usernameSelect, password1, userListFile = userListFile)
        HTML(sprintf("%s: Password changed", Sys.time()))
      } else {
        HTML(sprintf("%s: Different password entered for Password and Confirm Password", Sys.time()))
      }
    })
  })
  
  observeEvent(input$infoChangeBtn, {
    output$infoChangeMsg <- renderUI({
      user_base <- user_base()
      usernameSelect <- input$usernameSelect
      changeList <- list()
      for (x in setdiff(colnames(user_base), c('username', 'password_hash', 'permissions')))
      {
        if (sprintf('%sTxt', x) %in% names(input))
        {
          changeList[[x]] <- input[[sprintf('%sTxt', x)]]
        }
      }
      gChangeInfoNew(usernameSelect, changeList, userListFile = userListFile)
      msg <- sprintf("%s: Info changed for %s", Sys.time(), usernameSelect)
      
      
      return(HTML(msg))
    })
  })
  
  
}


# Page Config
#################################

zaWidgetUserManagementPageConfig <- list(
  
  # Disable Page
  # disabled = T,
  
  # Title for menu
  'title' = 'Account',
  
  # Sub-menu
  'submenu' = 'Settings',
  
  # Icon for menu
  'icon' = 'users', 
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('admin', 'guest')
)


