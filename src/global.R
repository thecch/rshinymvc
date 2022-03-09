 

### Source the file to load the 

# Import Libraries
#################################

library(R.utils)
library(plyr)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinyjqui)

library(DBI)
library(glue)
library(DT)

library(stringr)
library(readr)

library(tidyverse)
library(jsonlite)
library(dplyr)
library(tidyr)

#######################################################################
provided <- function(data, condition, call) {
  if (rlang::eval_tidy(enquo(condition), data)) {
    rlang::eval_tidy(rlang::quo_squash(quo(data %>% !!enquo(call))))
  } else data
}

formatDTDisplay <- function(a, selectChoice = 'multiple', currencyCol = NULL, roundCol = NULL, roundDigit = 2, pagelen = 50, rownames = F) {
  a %>% datatable(#extensions = 'Buttons',
    selection = selectChoice, rownames = rownames, filter = 'top', escape = F,
    options = list( pageLength = pagelen, scrollX = T, scrollY = "500px", dom = 'T<"clear">lBfrtip')
  ) %>%
    provided(!is.null(currencyCol), formatCurrency(currencyCol, currency = "", interval = 3, mark = ",")) %>%
    provided(!is.null(roundCol), formatRound(roundCol, digits = roundDigit))
}



#######################################################################
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"



# App Data
#################################

# App Initialization
#################################
# Modules

# Widgets
sourceDirectory('src/components/widgets', modifiedOnly = F)

# Pages
sourceDirectory('src/pages/', modifiedOnly = F, recursive = T)

# Page Router
source('src/components/layout/AppPages.R')

# App Layout
sourceDirectory('src/components/layout/', modifiedOnly = F)

# Scripts

# test line
