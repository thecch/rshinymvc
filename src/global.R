

nodename <- Sys.info()[['nodename']]

# Import Libraries
#################################

# Base Packages
# Use check_package('package_name') to check if it exist before adding it here.
# This will reload the start-up time of shiny significantly.
# No need to load package if there's minimal usage of it.
# Instead add it to hidden and call it via <package_name>::
base_dependencies <- list(
  # Check for attached Packages with
  # names(sessionInfo()$otherPkgs)
  'attached' = c(
    'rlang',
    'shiny', 'shinydashboard', 'DT',
    'tidyverse'
  ),
  'hidden' = c(
    'shinyjqui', 'shinyWidgets', 'shinydashboardPlus', 'dashboardthemes', 'sodium'
  )
)

# Module Packages
module_dependencies <- c()

# App Packages
app_dependencies <- c('plotly')

# Load Packages
# Faster load time compared to loading it individually
suppressWarnings(suppressMessages(invisible(
  lapply(c(base_dependencies$attached, module_dependencies, app_dependencies), library, character.only = T, quietly = T)
)))

options(readr.show_col_types = F)



# Generic Functions
#######################################################################
`%!in%` <- Negate(`%in%`)

all_avail_packages <- function() {
  unlist(base_dependencies) %>%
    c(module_dependencies, app_dependencies) %>%
    tools::package_dependencies(recursive = T, db = installed.packages()) %>%
    unlist() %>%
    unique()
}

check_package <- function(x) {
  x %in% all_avail_packages()
}

provided <- function(data, condition, call, call2 = NULL) {
  condition <- ifelse(is.logical(condition), condition, rlang::eval_tidy(rlang::enquo(condition), data))
  
  if (condition) {
    rlang::eval_tidy(rlang::quo_squash(rlang::quo(data %>% !!rlang::enquo(call))))
  } else if (!is.null(call2)) {
    rlang::eval_tidy(rlang::quo_squash(rlang::quo(data %>% !!rlang::enquo(call2))))
  } else data
}

formatDTDisplay <- function(a, selectChoice = 'multiple', currencyCol = NULL, roundCol = NULL, roundDigit = 2, pagelen = 50, rownames = F) {
  a %>%
    DT::datatable(
      selection = selectChoice, rownames = rownames, filter = 'top', escape = F,
      options = list(pageLength = pagelen, scrollX = T, scrollY = "500px", dom = 'T<"clear">lBfrtip')
    ) %>%
    provided(!is.null(currencyCol), DT::formatCurrency(currencyCol, currency = "", interval = 3, mark = ",")) %>%
    provided(!is.null(roundCol), DT::formatRound(roundCol, digits = roundDigit))
}

modify_stop_propagation <- function(x) {
  x$children[[1]]$attribs$onclick = "event.stopPropagation()"
  x
}

create_btns <- function(x, ns = NS(''), username = NULL, admin = F) {
  if (admin) {
    x %>%
      purrr::map_chr(~ as.character(
        shiny::actionButton(ns(paste0('reset_', .x)), '', icon = shiny::icon('key'), class = 'btn-warning', onclick = 'get_id(this.id)') 
      ))
  } else {
    x %>%
      purrr::map_chr(~ as.character(
        shiny::div(class = "btn-group",
          if (shiny::isTruthy(.x)) {
            shiny::actionButton(ns(paste0('edit_', .x)), '', icon = shiny::icon('edit'), class = 'btn-info', onclick = 'get_id(this.id)')    
          } else {
            shiny::actionButton(ns(paste0('new_', .x)), '', icon = shiny::icon('plus'), class = 'btn-success', onclick = 'get_id(this.id)')
          },
          if (shiny::isTruthy(.x)) {
            shiny::actionButton(ns(paste0('reset_', .x)), '', icon = shiny::icon('key'), class = 'btn-warning', onclick = 'get_id(this.id)')
          },
          if (.x != username && shiny::isTruthy(.x)) {
            shiny::actionButton(ns(paste0('delete_', .x)), '', icon = shiny::icon('trash-alt'), class = 'btn-danger', onclick = 'get_id(this.id)')
          }
        )
      ))
  }
}



# Javascript Addons
#######################################################################
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"



# App Static Data
#################################




# App Initialization
#################################

# Modules
R.utils::sourceDirectory('src/components/modules', modifiedOnly = F, recursive = T)

# Widgets
source('src/components/widgets/widgets.R')

# Pages
R.utils::sourceDirectory('src/pages/', modifiedOnly = F, recursive = T)

# Page Router
source('src/components/layout/AppPages.R')

# App Layout
R.utils::sourceDirectory('src/components/layout/', modifiedOnly = F)

# Scripts










# End of script