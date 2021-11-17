#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny shinydashboard
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs onclick enable disable
#' @importFrom DT renderDataTable
#' @importFrom utils tail
#' @noRd
app_server <- function(input, output, session) {

  ### the start interface 
  
  data_load <- reactiveVal(NA)
  
  shinyjs::onclick("homeclick", { # going to loading user data interface
    updateSelectInput(session, "select_example_data", NULL,
                      choices = c("Please select the data..." = "no_data",
                                  "covid-19 infection cases in Poland" = "covid_poland",
                                  "HIV cases and deaths" = "deaths_and_new_cases_hiv"),
                      selected = "no_data")
  })
  
  shinyjs::onclick("to_user_data_button", { # going to loading user data interface
    new_interface <- switch(input[["interfaces"]],
                     "start" = "user_data",
                     "user_data" = "start")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
    data_load("user_data")
  })
  
  shinyjs::onclick("to_example_data_button", { # going to loading example data interface
    new_interface <- switch(input[["interfaces"]],
                            "start" = "example_data",
                            "example_data" = "start")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
    data_load("example_data")
  })
  
  ### the loading user data interface
  
  shinyjs::onclick("return_to_start_button1", { # back to the start interface
    new_interface <- switch(input[["interfaces"]],
                            "user_data" = "start",
                            "start" = "user_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  shinyjs::onclick("to_view_data_button1", { # going to view data in a table interface
    new_interface <- switch(input[["interfaces"]],
                            "user_data" = "table_view",
                            "table_view" = "user_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  shinyjs::onclick("to_visualize_data_button1", { # going to data visualization interface
    new_interface <- switch(input[["interfaces"]],
                            "user_data" = "visualization",
                            "visualization" = "user_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  ### the loading example data interface
  
  shinyjs::onclick("return_to_start_button2", { # back to the start interface
    new_interface <- switch(input[["interfaces"]],
                            "example_data" = "start",
                            "start" = "example_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
    updateSelectInput(session, "select_example_data", NULL,
                      choices = c("Please select the data..." = "no_data",
                                  "covid-19 infection cases in Poland" = "covid_poland",
                                  "HIV cases and deaths" = "deaths_and_new_cases_hiv"),
                      selected = "no_data")
  })
  
  dataset <- reactive({ # load data to analyse and visualization
    if(data_load() == "example_data"){
      example_data_name <- input[["select_example_data"]]
      if(example_data_name != "no_data"){
        e <- new.env()
        name <- load(file.path("data", paste0(example_data_name, ".rda")), envir = e)
        
        # display a confirmation
        shinyalert::shinyalert("Data loaded successfully",
                               type = "success",
                               confirmButtonText = "OK",
                               confirmButtonCol = "#a6a6a6")
        
        return(e[[name]])
      }else{
        return(NULL)
      }
    }
  })
  
  observeEvent(input[["select_example_data"]], { # when example data isn't choose, buttons to view and visualization interfaces are disabled
    if((input[["select_example_data"]] != "no_data") && (!is.null(dataset()))){
      shinyjs::enable("to_view_data_button2")
      shinyjs::enable("to_visualize_data_button2")
    }else{
      shinyjs::disable("to_view_data_button2")
      shinyjs::disable("to_visualize_data_button2")
    }
  })
  
  shinyjs::onclick("to_view_data_button2", { # going to view data in a table interface
    new_interface <- switch(input[["interfaces"]],
                            "example_data" = "table_view",
                            "table_view" = "example_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  shinyjs::onclick("to_visualize_data_button2", { # going to data visualization interface
    new_interface <- switch(input[["interfaces"]],
                            "example_data" = "visualization",
                            "visualization" = "example_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  ### the view data in a table interface
  
  shinyjs::onclick("return_to_select_data_button1", { # back to loading example or user data interface
    if(data_load() == "example_data"){
      new_interface <- switch(input[["interfaces"]],
                              "table_view" = "example_data",
                              "example_data" = "table_view")
    }else{
      new_interface <- switch(input[["interfaces"]],
                              "table_view" = "user_data",
                              "user_data" = "table_view")
    }
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  output[["data_table"]] <- DT::renderDataTable({
    utils::tail(dataset(), 10)
  },
  options = list(scrollX = TRUE,
                 deferRender = TRUE,
                 paging = TRUE,
                 searching = FALSE,
                 pageLength = 5,
                 lengthMenu = c(5, 10),
                 server = TRUE
  ), 
  rownames = FALSE
  )
  
  ### the data visualization interface
  
  shinyjs::onclick("return_to_select_data_button2", { # back to loading example or user data interface
    if(data_load() == "example_data"){
      new_interface <- switch(input[["interfaces"]],
                              "visualization" = "example_data",
                              "example_data" = "visualization")
    }else{
      new_interface <- switch(input[["interfaces"]],
                              "visualization" = "user_data",
                              "user_data" = "visualization")
    }
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  ### the data visualization and prediction interface
 
  shinyjs::onclick("return_to_visualization_data_button", { # back to visualization data interface
    new_interface <- switch(input[["interfaces"]],
                            "prediction" = "visualization",
                            "visualization" = "prediction")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
}
