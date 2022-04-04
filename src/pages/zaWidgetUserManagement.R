


zaWidgetUserManagementModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(width = 12,  status = "primary",
      tabsetPanel(
        tabPanel('UserList',
          actionButton(ns('refreshUser'), 'Refresh'),
          DTOutput(ns('user_base'))
        ),
        tabPanel('Change',
          uiOutput(ns('ChangeForm')),
          uiOutput(ns('infoChangeMsg')),
          uiOutput(ns('uTypeChangeMsg')),
          uiOutput(ns('addUMsg')),
          uiOutput(ns('delUMsg')),
          uiOutput(ns('passordChangeMsg')),
          uiOutput(ns('ChangeFormDetails'))
        )
      )
    )
  )
}

zaWidgetUserManagementModule <- function(input,  output,  session,  ...) {

  ns <- session$ns
  env_bind(parent.env(environment()), ...)
  credentials <- reactive({ req(session$userData$credentials()) })
  username <- reactive({ req(credentials()$info$username) })
  observe({ appendAccessLog(username(), getwd(), session$ns('name'), '', '') })
  
  
  # Module Data
  ###############################
  
  user_base <- reactive({
    input$refreshUser
    getUserBase()
  })
  
  output$user_base <- renderDT({
    user_base() %>%
      select('username', 'name', 'permissions') %>%
      formatDTDisplay()
  })
  
  output$ChangeForm <- renderUI({
    user_base <- user_base()
    username <- credentials()$info$username
    
    inputPanel(
      selectInput(
        ns('usernameSelect'), 'Select User',
        ifelse(req(credentials()$info$permissions) == 'admin', user_base, filter(user_base, `username` == username)),
        username, selectize = F
      )
    )
  })
  
  output$ChangeFormDetails <- renderUI({
    
    user_base <- user_base() %>%
      filter(`username` == input$usernameSelect)
    uList <- setdiff(user_base$username, credentials()$info$username)
    
    tabsetPanel(
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
  })
  
  observeEvent(input$uDelBtn, {
    if (credentials()$info$permissions == 'admin') {
      if (sodium::password_verify(credentials()$info$password_hash, input$delAdminPw)) {
        deleteUser(input$delUsername)
        msg <- sprintf("%s: Deleted user %s", Sys.time(), input$delUsername)
      } else {
        msg <- sprintf("%s: Requestor password incorrect", Sys.time())
      }
    } else {
      msg <- sprintf("%s: Requestor does not have admin rights", Sys.time())
    }
    
    output$delUMsg <- renderUI({
      HTML(msg)
    })
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
  
  
  
  # Export Data
  #####################
  dataExport <- reactiveValues(
    'test' = 'Content'
  )
  
  appData[[pageName]] <- reactive({ reactiveValuesToList(dataExport) })
}


# Page Config
#################################

zaWidgetUserManagementPageConfig <- list(
  
  # Disable Page
  disabled = T,
  
  # Title for menu
  'title' = 'UserManagement', 
  
  # Icon for menu
  'icon' = 'users', 
  
  # Roles with permission to view page.
  # Exclusion will cause user to be TOTALLY unable to view page
  # Partial permission will have to be controlled within module
  'permission' = c('admin',  'user',  'demo')
)


