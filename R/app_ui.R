#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#' @import shiny shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # adding external resources
    golem_add_external_resources(),
    # UI elements
    shinydashboard::dashboardPage(title = "time seRies visualization",
                                  shinydashboard::dashboardHeader(title = tagList(tags[["a"]](div(id = "homeclick",
                                                                                                  onclick = "openTab('appdesc')"),
                                                                                              href = NULL,
                                                                                              "VISUALIZATION AND ANALYSIS APP",
                                                                                              title = "Homepage",
                                                                                              class = "home"),
                                                                                  tags[["script"]](shiny::HTML( "var openTab = function(tabName){
                                                                                                                        $('a', $('.sidebar')).each(function() {
                                                                                                                        if(this.getAttribute('data-value') == tabName) {
                                                                                                                        this.click()
                                                                                                                        };
                                                                                                                        });
                                                                                                                        }"))), 
                                                                  titleWidth = 1400),
                                  shinydashboard::dashboardSidebar(disable = TRUE,
                                                                   shinydashboard::sidebarMenu(id = "interfaces",
                                                                                               shinydashboard::menuItem("The start interface", tabName = "start"),
                                                                                               shinydashboard::menuItem("Loading user data", tabName = "user_data"),
                                                                                               shinydashboard::menuItem("Loading example data", tabName = "example_data"),
                                                                                               shinydashboard::menuItem("View data in a table", tabName = "table_view"),
                                                                                               shinydashboard::menuItem("Data visualization", tabName = "visualization"),
                                                                                               shinydashboard::menuItem("Data visualization and prediction", tabName = "prediction"))),
                                  shinydashboard::dashboardBody(
                                    shinydashboard::tabItems(
                                      shinydashboard::tabItem(tabName = "start",
                                                              fluidRow()),
                                      shinydashboard::tabItem(tabName = "user_data",
                                                              fluidRow()),
                                      shinydashboard::tabItem(tabName = "example_data",
                                                              fluidRow()),
                                      shinydashboard::tabItem(tabName = "table_view", 
                                                              fluidRow()),
                                      shinydashboard::tabItem(tabName = "visualization", 
                                                              fluidRow()),
                                      shinydashboard::tabItem(tabName = "prediction", 
                                                              fluidRow()))))
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application.
#' 
#' @import shiny shinyjs shinyalert
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'tRis'
    ),
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert()
  )
}

