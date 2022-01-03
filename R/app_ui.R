#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#' @import shiny shinydashboard
#' @importFrom DT dataTableOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs useShinyjs disabled
#' @noRd
#' 
app_ui <- function(request) {
  tagList(
    # adding external resources
    golem_add_external_resources(),
    # UI elements
    shinydashboard::dashboardPage(title = "time seRies visualization",
                                  shinydashboard::dashboardHeader(title = tagList(tags[["a"]](div(id = "homeclick",
                                                                                                  "VISUALIZATION AND ANALYSIS APP",
                                                                                                  onclick = "openTab('start')"),
                                                                                              href = NULL,
                                                                                              NULL,
                                                                                              title = "Homepage",
                                                                                              class = "home"),
                                                                                  tags[["script"]](shiny::HTML( "var openTab = function(tabName){
                                                                                                                        $('a', $('.sidebar')).each(function() {
                                                                                                                        if(this.getAttribute('data-value') == tabName) {
                                                                                                                        this.click()
                                                                                                                        };
                                                                                                                        });
                                                                                                                        }"))), 
                                                                  titleWidth = "100%"),
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
                                                              fluidRow(column(12, align = "center", 
                                                                              div(id = "start-text", includeMarkdown(file.path("inst", "app", "App_description.md"))))),
                                                              br(),
                                                              fluidRow(column(6, div(id = "box-button-right", add_helper(actionButton("to_user_data_button", shiny::HTML("load own <br/> data")), "User_data"))),
                                                                       column(6, div(id = "box-button-left", add_helper(actionButton("to_example_data_button", shiny::HTML("select example <br/> data")), "Example_data"))))),
                                      shinydashboard::tabItem(tabName = "user_data",
                                                              fluidRow(column(12, align = "center",
                                                                              selectInput("select_filetype", NULL,
                                                                                          choices = c("Please select a type of file..." = "no_type", 
                                                                                                      ".csv" = "csv",
                                                                                                      ".txt" = "txt",
                                                                                                      ".xlsx" = "xlsx"),
                                                                                          selected = "no_type"))),
                                                              fluidRow(column(12, align = "center",
                                                                              selectInput("select_separator", NULL,
                                                                                           choices=c("Please select a separator..." = "no_sep", 
                                                                                                     'Comma'=',',
                                                                                                     'Semicolon'=';',
                                                                                                     'Tab'='\t',
                                                                                                     'Space'=' '),
                                                                                           selected='no_sep'))),
                                                              fluidRow(column(12, align = "center", div(id = "box-select-file", add_helper(uiOutput('select_file'), "Select_file")))),
                                                              fluidRow(column(12, align = "center", helpText('The maximum filesize is 1GB'))),
                                                              br(),
                                                              fluidRow(column(4, div(id = "box-button-right1", actionButton("return_to_start_button1", shiny::HTML("return to <br/> previous page"), class = "return-button"))),
                                                                       column(4, align = "center", div(id = "box-button-center1", add_helper(shinyjs::disabled(actionButton("to_view_data_button1", shiny::HTML("see the data <br/> in the table"), class = "view-and-vis-button")), "View_data"))),
                                                                       column(4, div(id = "box-button-left1", add_helper(shinyjs::disabled(actionButton("to_visualize_data_button1", shiny::HTML("see the data <br/> visualization"), class = "view-and-vis-button")), "Vizualization"))))),
                                      shinydashboard::tabItem(tabName = "example_data",
                                                              fluidRow(column(12, align = "center",
                                                                              selectInput("select_example_data", NULL,
                                                                                          choices = c("Please select the data..." = "no_data",
                                                                                                      "covid-19 infection cases in Poland" = "covid_poland",
                                                                                                      "HIV cases and deaths" = "deaths_and_new_cases_hiv"),
                                                                                          selected = "no_data"))),
                                                              br(),
                                                              fluidRow(column(4, div(id = "box-button-right2", actionButton("return_to_start_button2", shiny::HTML("return to <br/> previous page"), class = "return-button"))),
                                                                       column(4, align = "center", div(id = "box-button-center2", add_helper(actionButton("to_view_data_button2", shiny::HTML("see the data <br/> in the table"), class = "view-and-vis-button"), "View_data"))),
                                                                       column(4, div(id = "box-button-left2", add_helper(actionButton("to_visualize_data_button2", shiny::HTML("see the data <br/> visualization"), class = "view-and-vis-button"), "Vizualization"))))),
                                      shinydashboard::tabItem(tabName = "table_view",
                                                              fluidRow(column(12, add_helper(shinycssloaders::withSpinner(DT::dataTableOutput("data_table", width = 800), color="#efefef"), "Table_with_data"))),
                                                              br(),
                                                              fluidRow(column(12, align = "center", div(id = "box-button-return-select1", actionButton("return_to_select_data_button1", shiny::HTML("return to <br/> previous page"), class = "return-button"))))),
                                      shinydashboard::tabItem(tabName = "visualization",
                                                              fluidRow(column(12, align = "center", uiOutput("map_visualization"))),
                                                              br(),
                                                              fluidRow(column(6, div(id = "box-button-return-select2", actionButton("return_to_select_data_button2", shiny::HTML("return to <br/> previous page"), class = "return-button"))),
                                                                       column(6, div(id = "box-button-go-prediction", add_helper(shinyjs::disabled(actionButton("to_prediction_button", shiny::HTML("see time series <br/> and prediction"))), "Prediction"))))),
                                      shinydashboard::tabItem(tabName = "prediction", 
                                                              add_helper(shinycssloaders::withSpinner(plotOutput("prediction_plot", width="800px"), color = "#efefef"), "Plot_with_predictions"),
                                                              br(),
                                                              fluidRow(column(12, align = "center", div(id = "box-button-return-visualization", actionButton("return_to_visualization_data_button", shiny::HTML("return to <br/> previous page"), class = "return-button")))))))),
    tags[["footer"]]("Made as part of a BSc Thesis", class = "footer")
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application.
#' 
#' @import shiny 
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyalert useShinyalert
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
