#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  shinyjs::onclick("to_user_data_button", { # going to loading user data interface
    new_interface <- switch(input$interfaces,
                     "start" = "user_data",
                     "user_data" = "start")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  shinyjs::onclick("to_example_data_button", { # going to loading example data interface
    new_interface <- switch(input$interfaces,
                            "start" = "example_data",
                            "example_data" = "start")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  observeEvent(input$select_example_data, { # when example data isn't choose, buttons to view and visualization interfaces are disabled
    if(input$select_example_data != "no_data"){
      shinyjs::enable("to_view_data_button")
      shinyjs::enable("to_visualize_data_button")
    }else{
      shinyjs::disable("to_view_data_button")
      shinyjs::disable("to_visualize_data_button")
    }
  })
  
  shinyjs::onclick("to_view_data_button", { # going to view data in a table interface
    new_interface <- switch(input$interfaces,
                            "example_data" = "table_view",
                            "table_view" = "example_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  shinyjs::onclick("to_visualize_data_button", { # going to data visualization interface
    new_interface <- switch(input$interfaces,
                            "example_data" = "visualization",
                            "visualization" = "example_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
}
