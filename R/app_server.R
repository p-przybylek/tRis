#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny shinydashboard maps
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs onclick enable disable
#' @importFrom data.table as.data.table
#' @importFrom DT renderDataTable
#' @importFrom utils tail read.table read.csv
#' @importFrom openxlsx read.xlsx
#' @noRd
#' 
app_server <- function(input, output, session) {
  
  ### option to increase input limit to 1GB
  
  options(shiny.maxRequestSize = 1024*1024^2)

  ### the start interface 
  
  data_load <- reactiveVal(NA)
  
  shinyjs::onclick("homeclick", { # reset app
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
    
    # return all inputs and buttons to default state
    updateSelectInput(session, "select_filetype", NULL,
                      choices = c("Please select a type of file..." = "no_type", 
                                  ".csv" = "csv",
                                  ".txt" = "txt",
                                  ".xlsx" = "xlsx"),
                      selected = "no_type")
    updateSelectInput(session, "select_separator", NULL,
                choices=c("Please select a separator..." = "no_sep", 
                          'Comma'=',',
                          'Semicolon'=';',
                          'Tab'='\t',
                          'Space'=' '),
                selected='no_sep')
    shinyjs::disable("to_view_data_button1")
    shinyjs::disable("to_visualize_data_button1")
    
    
  })
  
  output[["select_file"]] <- renderUI({ # render select file

    # validate extension list based on the chosen filetype
    extension_list <- switch(input[["select_filetype"]],
                             "csv" = c("text/csv",'csv'),
                             "txt" = c("text/plain",".txt"),
                             "xlsx" = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx"),
                             "no_type" = c("text/csv",'csv', "text/plain",".txt","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx")
                             )

    fileInput("select_file", NULL,
              accept = extension_list,
              buttonLabel = "Upload",
              placeholder = "Please choose a file...")

  })
  
  observeEvent(input[["return_to_start_button1"]],{ # reload fileInput when user returns to start panel
    output[["select_file"]] <- renderUI({

      # validate extension list based on the chosen filetype
      extension_list <- switch(input[["select_filetype"]],
                               "csv" = c("text/csv",'csv'),
                               "txt" = c("text/plain",".txt"),
                               "xlsx" = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx"),
                               "no_type" = c("text/csv",'csv', "text/plain",".txt","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx")
      )
      fileInput("select_file", NULL,
                accept = extension_list,
                buttonLabel = "Upload",
                placeholder = "Please choose a file...")
      
    })
    
  })

  observeEvent({input[["select_filetype"]]}, { # when filetype is xlsx, separator options are disabled
      if(input[["select_filetype"]]=='xlsx'){
        shinyjs::disable("select_separator")
      }else{
        shinyjs::enable("select_separator")
      }
    })
  
  observe({ # when filetype and separator aren't chosen, file select is disabled
    if(((input[["select_filetype"]] != "no_type") && (input[["select_separator"]] != "no_sep" ))||
          input[["select_filetype"]]== "xlsx"){
      shinyjs::enable("select_file")
    }else{
      shinyjs::disable("select_file")
    }
  })
  
  observeEvent(input[["select_file"]], { # when example data isn't choosen, buttons to view and visualization interfaces are disabled
    if(!is.null(dataset())){
      shinyjs::enable("to_view_data_button1")
      shinyjs::enable("to_visualize_data_button1")
    }else{
      shinyjs::disable("to_view_data_button1")
      shinyjs::disable("to_visualize_data_button1")
    }
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
    
    # return select input to default state
    updateSelectInput(session, "select_example_data", NULL,
                      choices = c("Please select the data..." = "no_data",
                                  "covid-19 infection cases in Poland" = "covid_poland",
                                  "HIV cases and deaths" = "deaths_and_new_cases_hiv"),
                      selected = "no_data")
  })
  
  observeEvent(input[["select_example_data"]], { # when example data isn't choosen, buttons to view and visualization interfaces are disabled
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
  
  ### loading a dataset

  dataset <- reactive({ # load data to analyse and visualize
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
    if(data_load() == "user_data"){
      data_file <- input[["select_file"]]
      if(is.null(data_file)){return(NULL)}
      
      # check filetype and alert if invalid
      filetype <- input[['select_filetype']]
      if(filetype == 'csv'){
          if( data_file$type == "text/csv"){
            user_dataset <- utils::read.csv(file=data_file$datapath, sep=input[['select_separator']], header=TRUE)
            
            # display a confirmation
            shinyalert::shinyalert("Data loaded successfully",
                                   type = "success",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            
            # enable buttons
            shinyjs::enable("to_view_data_button1")
            shinyjs::enable("to_visualize_data_button1")
          }
          else{
            user_dataset <- NULL
            
            # display error
            shinyalert::shinyalert("Invalid extension",
                                   "Please choose a .csv file or a valid extension type for your file.",
                                   type = "error",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            
            # disable buttons
            shinyjs::disable("to_view_data_button1")
            shinyjs::disable("to_visualize_data_button1")
          }

        }
        else if(filetype == 'txt'){

          if( data_file$type == "text/plain"){
            user_dataset <- utils::read.table(file=data_file$datapath, sep=input[['select_separator']], header=TRUE)
            
            # display a confirmation
            shinyalert::shinyalert("Data loaded successfully",
                                   type = "success",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            # enable buttons
            shinyjs::enable("to_view_data_button1")
            shinyjs::enable("to_visualize_data_button1")
          }
          else{
            user_dataset <- NULL
            
            # display error
            shinyalert::shinyalert("Invalid extension",
                                   "Please choose a .txt file or a valid extension type for your file.",
                                   type = "error",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            
            # disable buttons
            shinyjs::disable("to_view_data_button1")
            shinyjs::disable("to_visualize_data_button1")
          }
        }
        else if(filetype == 'xlsx'){

          if( data_file$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
            user_dataset <- openxlsx::read.xlsx(data_file$datapath)
            
            # display a confirmation
            shinyalert::shinyalert("Data loaded successfully",
                                   type = "success",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            
            # enable buttons
            shinyjs::enable("to_view_data_button1")
            shinyjs::enable("to_visualize_data_button1")
          }
          else{
            user_dataset <- NULL
            
            # display error
            shinyalert::shinyalert("Invalid extension",
                                   "Please choose a .xlsx file or a valid extension type for your file.",
                                   type = "error",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            
            # disable buttons
            shinyjs::disable("to_view_data_button1")
            shinyjs::disable("to_visualize_data_button1")

          }
          
        }
      else{
        user_dataset <- NULL
        
        # display error
        shinyalert::shinyalert("Invalid extension type",
                               type = "error",
                               confirmButtonText = "OK",
                               confirmButtonCol = "#a6a6a6")
        
        # disable buttons
        shinyjs::disable("to_view_data_button1")
        shinyjs::disable("to_visualize_data_button1")
      }
        return(as.data.table(user_dataset))
      }
  })
  
  ### the view data in a table interface
  
  shinyjs::onclick("return_to_select_data_button1", { # back to loading example or user data interface
    if(data_load() == "example_data"){
      new_interface <- switch(input[["interfaces"]],
                              "table_view" = "example_data",
                              "example_data" = "table_view")
    }else if(data_load() == "user_data"){
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
    }else if(data_load() == "user_data"){
      new_interface <- switch(input[["interfaces"]],
                              "visualization" = "user_data",
                              "user_data" = "visualization")
    }
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  shinyjs::onclick("to_prediction_button", { # going to data visualization and prediction interface
    new_interface <- switch(input[["interfaces"]],
                            "visualization" = "prediction",
                            "prediction" = "visualization")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  output[["map_visualization"]] <- renderUI( # creating data visualization UI
    sidebarLayout(
      sidebarPanel(
        fluidRow(column(12, align = "center", selectInput("select_data_type", shiny::HTML("Please choose what <br/> your data is about:"),
                                                          choices = c("World", "Poland")))),
        fluidRow(column(12, align = "center", selectInput("select_geo_column", shiny::HTML("Please select column <br/> contains geographic data:"),
                                                          choices = c("no column", colnames(dataset())),
                                                          selected = "no column"))),
        fluidRow(column(12, align = "center", selectInput("select_time_column", shiny::HTML("Please select column <br/> contains time data:"),
                                                          choices = c("no column", colnames(dataset())),
                                                          selected = "no column"))),
        fluidRow(column(12, align = "center", selectInput("select_measurements_column", shiny::HTML("Please select column <br/> contains measurements:"),
                                                          choices = c("no column", colnames(dataset())),
                                                          selected = "no column"))),
        width = 3),
      mainPanel(
        div(id="box-mapplot",shinycssloaders::withSpinner(leafletOutput("map_plot"), color = "#efefef")),
        width = 9)
    )
  )
  
  observeEvent(input[["select_geo_column"]], { # check selected column for geografic data
    if(input[["select_geo_column"]] != "no column"){
      vector_geo <- as.character(dataset()[[input[["select_geo_column"]]]])
      len <- unique(nchar(vector_geo))
      val <- FALSE
      if(input[["select_data_type"]] == "Poland"){
        e <- new.env()
        name <- load(file.path("data", "poland_teryt.rda"), envir = e)
        teryt <- e[[name]]$teryt
        if(length(len) != 1 && length(len) != 2){
          val <- TRUE
        }else{
          if(!all(len %in% c(2,4)) && !all(len %in% c(3,5))){
            val <- TRUE
          }else{
            if(all(len %in% c(2,4))){
              if(all(is.na(as.numeric(vector_geo))) || !all(vector_geo %in% c("00", "0000", teryt))){
                val <- TRUE
              }
            }else if(all(len %in% c(3,5))){
              vector_geo_new <- substr(vector_geo, 2, nchar(vector_geo))
              if(all(is.na(as.numeric(vector_geo_new))) || !all(vector_geo_new %in% c("00", "0000", teryt))){
                val <- TRUE
              }
            }
          }
        }
        if(val){
          
          # display error
          shinyalert::shinyalert("Invalid column type",
                                 "The selected column for geografic data contains an invalid format. Please select a column that contains the one of the TERYT format (tXX and tXXXX or XX and XXXX) or change type od data for 'World'.",
                                 type = "error",
                                 confirmButtonText = "OK",
                                 confirmButtonCol = "#a6a6a6")
          
          # return select input to default state
          updateSelectInput(session, "select_geo_column", 
                            shiny::HTML("Please select column <br/> contains geographic data:"),
                            choices = c("no column", colnames(dataset())),
                            selected = "no column")
        }
      }else if(input[["select_data_type"]] == "World"){ # properly format of a column is ISO 3166-1
        if((length(len) != 1 && length(len) != 2) || !all(len %in% c(2,3,8))){
          val <- TRUE
        }else{
          if(all(len %in% c(2,8)) && !all(vector_geo %in% c("OWID_WRL", maps::iso3166$a2))){
            val <- TRUE
          }else if(all(len %in% c(3,8)) && (!all(vector_geo %in% c("OWID_WRL", maps::iso3166$a3)) || !all(vector_geo %in% c("OWID_WRL", maps::iso3166$a3)))){
            val <- TRUE
          }else if(len == 8 && (length(len) == 1)){
            val <- TRUE
          }
        }
        if(val){
          
          # display error
          shinyalert::shinyalert("Invalid column type",
                                 "The selected column for geografic data contains an invalid format. Please select a column that contains the one of the ISO 3166-1 format or change type od data for 'Poland'.",
                                 type = "error",
                                 confirmButtonText = "OK",
                                 confirmButtonCol = "#a6a6a6")
          
          # return select input to default state
          updateSelectInput(session, "select_geo_column", 
                            shiny::HTML("Please select column <br/> contains geographic data:"),
                            choices = c("no column", colnames(dataset())),
                            selected = "no column")
        }
      }
    }
  })
  
  observeEvent(input[["select_time_column"]], { # check selected column for time data
    if(input[["select_time_column"]] != "no column"){
      vector_time <- as.character(dataset()[[input[["select_time_column"]]]])
      len <- unique(nchar(vector_time))
      val <- FALSE
      if(length(len) != 1 || (len != 4 && len != 10)){
        val <- TRUE
      }else{
        if(len == 10){
          first_option <- all(is.na(as.Date(vector_time, format="%Y-%m-%d")))
          second_option <- all(is.na(as.Date(vector_time, format="%Y.%m.%d")))
          if(first_option && second_option){
            val <- TRUE
          }
        }else{
          if(isFALSE(all(as.integer(vector_time) >= 1000)) || isFALSE(all(as.integer(vector_time) <= as.integer(format(Sys.Date(), "%Y"))))){
            val <- TRUE
          }
        }
      }
      if(val){
        
        # display error
        shinyalert::shinyalert("Invalid column type",
                               "The selected column for dates contains an invalid format. Please select a column that contains the date format consistent with the one adopted by the application: YYYY or YYYY-MM-DD or YYYY.MM.DD.",
                               type = "error",
                               confirmButtonText = "OK",
                               confirmButtonCol = "#a6a6a6")
        
        # return select input to default state
        updateSelectInput(session, "select_time_column", 
                          shiny::HTML("Please select column <br/> contains time data:"),
                          choices = c("no column", colnames(dataset())),
                          selected = "no column")
      }
    }
  })
  
  observeEvent(input[["select_measurements_column"]], { # check selected column for measurements
    if(input[["select_measurements_column"]] != "no column"){
      if(class(dataset()[[input[["select_measurements_column"]]]]) != "numeric" && class(dataset()[[input[["select_measurements_column"]]]]) != "integer"){
        
        # display error
        shinyalert::shinyalert("Invalid column type",
                               "The selected column for measurements contains non-numeric values. Please select a column with numeric values.",
                               type = "error",
                               confirmButtonText = "OK",
                               confirmButtonCol = "#a6a6a6")
        
        # return select input to default state
        updateSelectInput(session, "select_measurements_column", 
                          shiny::HTML("Please select column <br/> contains measurements:"),
                          choices = c("no column", colnames(dataset())),
                          selected = "no column")
      } 
    }
  })
  
  output[["map_plot"]] <- renderLeaflet({ # ploting map
    validate(
      need(input[["select_geo_column"]] != "no column" && input[["select_time_column"]] != "no column" && input[["select_measurements_column"]] != "no column",
           "No columns selected. Please select columns containing time data, geographic data and measurements.")
    )
    plot_map(dataset(), input[["select_data_type"]], input[["select_geo_column"]], input[["select_time_column"]], input[["select_measurements_column"]])
  })
  
  ### the data visualization and prediction interface
 
  shinyjs::onclick("return_to_visualization_data_button", { # back to visualization data interface
    new_interface <- switch(input[["interfaces"]],
                            "prediction" = "visualization",
                            "visualization" = "prediction")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
}
