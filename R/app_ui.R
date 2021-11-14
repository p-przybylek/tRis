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
                                                                                                  onclick = "openTab('start')"),
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
                                                              fluidRow(column(12, includeMarkdown(file.path("inst", "app", "App_description.md")))),
                                                              fluidRow(column(6, div(id = "box-button-right", actionButton("to_user_data_button", shiny::HTML("load own <br/> data"), class = "start-button"))),
                                                                       column(6, div(id = "box-button-left", actionButton("to_example_data_button", shiny::HTML("select example <br/> data"), class = "start-button"))))),
                                      shinydashboard::tabItem(tabName = "user_data",
                                                              fluidRow()),
                                      shinydashboard::tabItem(tabName = "example_data",
                                                              fluidRow(column(12, align = "center",
                                                                              selectInput("select_example_data", NULL,
                                                                                          choices = c("Please select the data..." = "no_data",
                                                                                                      "covid-19 infection cases in Poland" = "covid_poland",
                                                                                                      "HIV cases and deaths" = "deaths_and_new_cases_hiv"),
                                                                                          selected = "no_data"))),
                                                              fluidRow(column(6, div(id = "box-button-right", actionButton("to_view_data_button", shiny::HTML("see the data <br/> in the table"), class = "view-and-vis-button"))),
                                                                       column(6, div(id = "box-button-left", actionButton("to_visualize_data_button", shiny::HTML("see the data <br/> visualization"), class = "view-and-vis-button"))))),
                                      shinydashboard::tabItem(tabName = "table_view",
                                                              fluidRow(),
                                                              fluidRow(column(12, div(id = "box-button-right", actionButton("return_to_select_data_button", shiny::HTML("return to previous page"), class = "return-button"))))),
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

