#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny shinydashboard maps forecast ggplot2 magrittr
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs onclick enable disable disabled refresh
#' @importFrom data.table as.data.table
#' @importFrom DT renderDataTable
#' @importFrom utils tail read.table read.csv
#' @importFrom openxlsx read.xlsx
#' @importFrom leaflet renderLeaflet leafletProxy addAwesomeMarkers setView awesomeIcons clearMarkers
#' @importFrom shinyhelper observe_helpers
#' @importFrom stats na.omit
#' 
#' @noRd
#' 
app_server <- function(input, output, session) {
  
  ### option to increase input limit to 1GB
  
  options(shiny.maxRequestSize = 1024*1024^2)
  
  ### add helpfiles to app
  
  shinyhelper::observe_helpers(help_dir = "inst/app/helpfiles")

  ### the start interface 
  
  data_load <- reactiveVal(NA)
  
  shinyjs::onclick("homeclick", { # reset app
    shinyjs::refresh()
  })
  
  shinyjs::onclick("to_user_data_button", { # going to loading user data interface
    new_interface <- switch(input[["interfaces"]],
                     "start" = "user_data",
                     "user_data" = "start")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
    data_load("user_data")
    data_example_name(NA)
  })
  
  shinyjs::onclick("to_example_data_button", { # going to loading example data interface
    new_interface <- switch(input[["interfaces"]],
                            "start" = "example_data",
                            "example_data" = "start")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
    data_load("example_data")
    data_example_name(NA)
  })
  
  ### the loading user data interface
  
  shinyjs::onclick("return_to_start_button1", { # back to the start interface
    new_interface <- switch(input[["interfaces"]],
                            "user_data" = "start",
                            "start" = "user_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
    shinyjs::refresh()
  })
  
  extensions<-reactiveValues(list=NA)
  
  output[["select_file"]] <- renderUI({ # render select file <-NAPISAĆ funkcję z dwóch poniższych fragmentów!

    # validate extension list based on the chosen filetype
    extensions$list<-switch(input[["select_filetype"]],
                             "csv" = c("text/csv",'csv'),
                             "txt" = c("text/plain",".txt"),
                             "xlsx" = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx"),
                             "no_type" = c("")
                             )

    shinyjs::disabled(
      fileInput("select_file", NULL,
              accept = extensions$list,
              buttonLabel = "Upload",
              placeholder = "Please choose a file...")
    )
    

  })

  observeEvent(input[["return_to_start_button1"]],{ # reload fileInput when user returns to start panel
    output[["select_file"]] <- renderUI({

      # validate extension list based on the chosen filetype
      extensions$list<-switch(input[["select_filetype"]],
                               "csv" = c("text/csv",'csv'),
                               "txt" = c("text/plain",".txt"),
                               "xlsx" = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx"),
                               "no_type" = c()
      )
      shinyjs::disabled(
        fileInput("select_file", NULL,
                  accept = extensions$list,
                  buttonLabel = "Upload",
                  placeholder = "Please choose a file...")
      )
      
    
      
    })
    
  })
  
  
  # observe({
  #   shinyjs::toggleState("select_separator", (input[["select_filetype"]] != 'xlsx'))
  # })


  observeEvent({input[["select_filetype"]]}, { # when filetype is xlsx, separator options are disabled
      if(input[["select_filetype"]]=='xlsx'){
        shinyjs::disable("select_separator")
      }else{
        shinyjs::enable("select_separator")
      }
    })
  

  observe({ # when filetype and separator aren chosen, file select is enabled
    if(((input[["select_filetype"]] != "no_type") && (input[["select_separator"]] != "no_sep" )) || input[["select_filetype"]]== "xlsx"){
      shinyjs::enable("select_file")
      extensions$list<-switch(input[["select_filetype"]],
                               "csv" = c("text/csv",'csv'),
                               "txt" = c("text/plain",".txt"),
                               "xlsx" = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx"),
                               "no_type" = c("")
                               
      )
      fileInput("select_file", NULL,
                accept = extensions$list,
                buttonLabel = "Upload",
                placeholder = "Please choose a file...")
    }else{
      shinyjs::disable("select_file")
    }
  })
  
  ### navigating
  


  
  
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
  
  data_example_name <- reactiveVal(NA)
  
  shinyjs::onclick("return_to_start_button2", { # back to the start interface
    new_interface <- switch(input[["interfaces"]],
                            "example_data" = "start",
                            "start" = "example_data")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
    shinyjs::refresh()
  })
  
  observeEvent(input[["select_example_data"]], { # when example data isn't choosen, buttons to view and visualization interfaces are disabled
    data_example_name(input[["select_example_data"]])
    if((input[["select_example_data"]] != "no_data") && (!is.null(dataset()))){
      shinyjs::enable("to_view_data_button2")
      shinyjs::enable("to_visualize_data_button2")
    }else{
      shinyjs::disable("to_view_data_button2")
      shinyjs::disable("to_visualize_data_button2")
    }
  })
  
  observe({
    shinyjs::toggleState("to_view_data_button2", (input[["select_example_data"]] != "no_data") && (!is.null(dataset())))
  })
  
  observe({
    shinyjs::toggleState("to_visualize_data_button2", (input[["select_example_data"]] != "no_data") && (!is.null(dataset())))
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
        
        output[["map"]] <- renderLeaflet(NULL)
        return(e[[name]])
      }else{
        return(NULL)
      }
    }
    if(data_load() == "user_data"){
      data_file <- input[["select_file"]]
      if(is.null(data_file)){return(NULL)}
      
      # check filetype and alert if invalid
      filetype <- input[["select_filetype"]]
      if(filetype == "csv"){
          if(data_file$type == "text/csv"){
            user_dataset <- utils::read.csv(file=data_file$datapath, sep=input[["select_separator"]], header=TRUE, encoding = "UTF-8")
            
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
        else if(filetype == "txt"){

          if( data_file$type == "text/plain"){
            user_dataset <- utils::read.table(file=data_file$datapath, sep=input[["select_separator"]], header=TRUE)
            
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

          if(data_file$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
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
      output[["map"]] <- renderLeaflet(NULL)
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
    utils::tail(dataset(), 20)
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
    
    leaflet::leafletProxy("map") %>% clearMarkers()
    shinyjs::disable("to_prediction_button")
    output[["prediction_plot"]] <- renderPlotly(NULL)
  })
  
  shinyjs::onclick("to_prediction_button", { # going to data visualization and prediction interface
    new_interface <- switch(input[["interfaces"]],
                            "visualization" = "prediction",
                            "prediction" = "visualization")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  output[["map_visualization"]] <- renderUI( # creating data visualization UI
    if(is.na(data_example_name())){
      sidebarLayout(
        sidebarPanel(
          fluidRow(column(12, align = "center", add_helper(selectInput("select_data_type", shiny::HTML("Please choose what your data is about:"),
                                                                                         choices = c("World", "Poland"),
                                                                                         selected = ifelse(data_example_name() == "covid_poland", "Poland", "World")), "Data_type"))),
          fluidRow(column(12, align = "center", add_helper(selectInput("select_geo_column", shiny::HTML("Please select column contains geographic data:"),
                                                                                         choices = c("no column", colnames(dataset())),
                                                                                         selected = ifelse(data_example_name() == "covid_poland", "territory", ifelse(data_example_name() == "deaths_and_new_cases_hiv", "Code", "no column"))), "Geo_column"))),
          fluidRow(column(12, align = "center", add_helper(selectInput("select_time_column", shiny::HTML("Please select column contains time data:"),
                                                                                         choices = c("no column", colnames(dataset())),
                                                                                         selected = ifelse(data_example_name() == "covid_poland", "date", ifelse(data_example_name() == "deaths_and_new_cases_hiv", "Year", "no column"))), "Time_column"))),
          fluidRow(column(12, align = "center", add_helper(selectInput("select_measurements_column", shiny::HTML("Please select column contains measurements:"),
                                                                       choices = c("no column", colnames(dataset())),
                                                                       selected = ifelse(data_example_name() == "covid_poland", "cases", ifelse(data_example_name() == "deaths_and_new_cases_hiv", "Deaths", "no column"))), "Measurement_column"))),
          width = 3),
        mainPanel(
          fluidRow(column(4, align = "left", uiOutput("radiobuttons_time_slider")),
                   column(8, align = "left", uiOutput("change_time_range"))),
          div(id="box-mapplot", add_helper(shinycssloaders::withSpinner(leafletOutput("map"), color = "#efefef"), "Map_plot")),
          width = 9)
      )
    }else{
      sidebarLayout(
        sidebarPanel(
          fluidRow(column(12, align = "center", add_helper(shinyjs::disabled(selectInput("select_data_type", shiny::HTML("Please choose what your data is about:"),
                                                                                         choices = c("World", "Poland"),
                                                                                         selected = ifelse(data_example_name() == "covid_poland", "Poland", "World"))), "Data_type"))),
          fluidRow(column(12, align = "center", add_helper(shinyjs::disabled(selectInput("select_geo_column", shiny::HTML("Please select column contains geographic data:"),
                                                                                         choices = c("no column", colnames(dataset())),
                                                                                         selected = ifelse(data_example_name() == "covid_poland", "territory", ifelse(data_example_name() == "deaths_and_new_cases_hiv", "Code", "no column")))), "Geo_column"))),
          fluidRow(column(12, align = "center", add_helper(shinyjs::disabled(selectInput("select_time_column", shiny::HTML("Please select column contains time data:"),
                                                                                         choices = c("no column", colnames(dataset())),
                                                                                         selected = ifelse(data_example_name() == "covid_poland", "date", ifelse(data_example_name() == "deaths_and_new_cases_hiv", "Year", "no column")))), "Time_column"))),
          fluidRow(column(12, align = "center", add_helper(selectInput("select_measurements_column", shiny::HTML("Please select column contains measurements:"),
                                                                       choices = c("no column", colnames(dataset())),
                                                                       selected = ifelse(data_example_name() == "covid_poland", "cases", ifelse(data_example_name() == "deaths_and_new_cases_hiv", "Deaths", "no column"))), "Measurement_column"))),
          width = 3),
        mainPanel(
          fluidRow(column(4, align = "left", uiOutput("radiobuttons_time_slider")),
                   column(8, align = "left", uiOutput("change_time_range"))),
          div(id="box-mapplot", add_helper(shinycssloaders::withSpinner(leafletOutput("map"), color = "#efefef"), "Map_plot")),
          width = 9)
      )
    }
  )
  
  observeEvent(input[["select_geo_column"]], { # check selected column for geographic data
    if(input[["select_geo_column"]] != "no column"){
      vector_geo <- as.character(dataset()[[input[["select_geo_column"]]]])
      if(input[["select_data_type"]] == "Poland"){
        if(any(unique(nchar(vector_geo)) %in% c(1,3))){
          vector_geo <- ifelse(nchar(vector_geo) == 1, paste0("0", vector_geo), vector_geo)
          vector_geo <- ifelse(nchar(vector_geo) == 3, ifelse(substr(vector_geo, 1,1) != "t", paste0("0", vector_geo), vector_geo), vector_geo)
        }
        val <- geo_column_check(vector_geo, "Poland")
        if(val){
          
          # display error
          shinyalert::shinyalert("Invalid column type",
                                 "The selected column for geografic data contains an invalid format. Please select a column that contains the one of the TERYT format (tXX and tXXXX or XX and XXXX) or change type od data for 'World'.",
                                 type = "error",
                                 confirmButtonText = "OK",
                                 confirmButtonCol = "#a6a6a6")
          
          # return select input to default state
          updateSelectInput(session, "select_geo_column", 
                            shiny::HTML("Please select column contains geographic data:"),
                            choices = c("no column", colnames(dataset())),
                            selected = "no column")
        }
      }else if(input[["select_data_type"]] == "World"){ # properly format of a column is ISO 3166-1
        val <- geo_column_check(vector_geo, "World")
        if(val){
          
          # display error
          shinyalert::shinyalert("Invalid column type",
                                 "The selected column for geografic data contains an invalid format. Please select a column that contains the one of the ISO 3166-1 format or change type od data for 'Poland'.",
                                 type = "error",
                                 confirmButtonText = "OK",
                                 confirmButtonCol = "#a6a6a6")
          
          # return select input to default state
          updateSelectInput(session, "select_geo_column", 
                            shiny::HTML("Please select column contains geographic data:"),
                            choices = c("no column", colnames(dataset())),
                            selected = "no column")
        }
      }
    }
  })
  
  observeEvent(input[["select_time_column"]], { # check selected column for time data
    if(input[["select_time_column"]] != "no column"){
      vector_time <- as.character(dataset()[[input[["select_time_column"]]]])
      val <- time_column_check(vector_time)
      if(val){
        
        # display error
        shinyalert::shinyalert("Invalid column type",
                               "The selected column for dates contains an invalid format. Please select a column that contains the date format consistent with the one adopted by the application: YYYY or YYYY-MM-DD or YYYY.MM.DD.",
                               type = "error",
                               confirmButtonText = "OK",
                               confirmButtonCol = "#a6a6a6")
        
        # return select input to default state
        updateSelectInput(session, "select_time_column", 
                          shiny::HTML("Please select column contains time data:"),
                          choices = c("no column", colnames(dataset())),
                          selected = "no column")
      }
    }
  })
  
  observeEvent(input[["select_measurements_column"]], { # check selected column for measurements
    if(input[["select_measurements_column"]] != "no column"){
      if(class(dataset()[[input[["select_measurements_column"]]]]) != "numeric" && class(dataset()[[input[["select_measurements_column"]]]]) != "integer" && all(is.na(dataset()[[input[["select_measurements_column"]]]]))){
        
        # display error
        shinyalert::shinyalert("Invalid column type",
                               "The selected column for measurements contains non-numeric values. Please select a column with numeric values.",
                               type = "error",
                               confirmButtonText = "OK",
                               confirmButtonCol = "#a6a6a6")
        
        # return select input to default state
        updateSelectInput(session, "select_measurements_column", 
                          shiny::HTML("Please select column contains measurements:"),
                          choices = c("no column", colnames(dataset())),
                          selected = "no column")
      } 
    }
  })
  
  observe(
    if(!is.null(input[["slider_time_range"]]) && !is.infinite(input[["slider_time_range"]])){
      output[["map"]] <- renderLeaflet({ # ploting map
        validate(
          need(input[["select_geo_column"]] != "no column" && input[["select_time_column"]] != "no column" && input[["select_measurements_column"]] != "no column",
               "No columns selected. Please select columns containing time data, geographic data and measurements.")
        )
        plot_map(isolate(dataset()), input[["select_data_type"]], input[["select_geo_column"]], input[["select_time_column"]], input[["select_measurements_column"]], input[["slider_time_range"]])
      })
    }
  )
  
  observeEvent(input[["select_time_column"]], {
    output[["radiobuttons_time_slider"]] <- renderUI({ # creating radio buttons for choosing time options for visualization
      if(input[["select_time_column"]] != "no column"){
        radioButtons("check_for_time_slider", "Show map for:", 
                     choices = c("A specific time value" = "one_value", 
                                 "Time period (sum of measurement)" = "period"), 
                     selected = "one_value")
      }
    })
  })
  
  observeEvent(input[["check_for_time_slider"]], { # creating slider for choosing a time range for visualization
    output[["change_time_range"]] <- renderUI({
      if(input[["select_time_column"]] != "no column"){
        vector_time <- isolate(dataset())[[input[["select_time_column"]]]]
        if(is.null(vector_time)) return(NULL)
        if(unique(nchar(as.character(vector_time))) == 4){
          if(input[["check_for_time_slider"]] == "one_value"){
            val <- max(vector_time)
            title <- "Time value for visualization:"
          }else{
            val <- c(max(vector_time), max(vector_time))
            title <- "Time range for visualization:"
          }
          sliderInput("slider_time_range", title, 
                      min = min(vector_time),
                      max = max(vector_time),
                      value = val,
                      sep = "",
                      step = abs(vector_time[1]-vector_time[2]))
        }else{
          format <- ifelse(substr(vector_time[1],5,5) == "-", "%Y-%m-%d", "%Y.%m.%d")
          if(input[["check_for_time_slider"]] == "one_value"){
            val <- as.Date(max(vector_time))
            title <- "Time value for visualization:"
          }else{
            val <- c(as.Date(max(vector_time)), as.Date(max(vector_time)))
            title <- "Time range for visualization:"
          }
          sliderInput("slider_time_range", title, 
                      min = as.Date(min(vector_time)),
                      max = as.Date(max(vector_time)),
                      value = val,
                      timeFormat = format)
        }
      }
    })
  })
  
  ### the data visualization and prediction interface
 
  shinyjs::onclick("return_to_visualization_data_button", { # back to visualization data interface
    new_interface <- switch(input[["interfaces"]],
                            "prediction" = "visualization",
                            "visualization" = "prediction")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
  prediction_area <- reactiveValues(data = NULL)
  
  observeEvent(input[["map_shape_click"]], {
    if(!is.null(input[["map_shape_click"]]$id)){
      
      # update map visualization
      zoom_lvl <- ifelse(input[["select_data_type"]] == "Poland", 8, 3)
      icon <- leaflet::awesomeIcons(icon = "circle", library = "fa", markerColor = "white", iconColor = "#efefef")
      leaflet::leafletProxy("map") %>%
        leaflet::clearMarkers() %>% 
        leaflet::setView(lng = input[["map_shape_click"]]$lng, lat = input[["map_shape_click"]]$lat, zoom = zoom_lvl) %>% 
        leaflet::addAwesomeMarkers(input[["map_shape_click"]]$lng, input[["map_shape_click"]]$lat, icon = icon, label = paste0("You have chosen: ", sub(".*\\;","", input[["map_shape_click"]]$id)))
      
      # create dataset for predictions
      area_code <- sub("\\;.*", "", input[["map_shape_click"]]$id)
      setkeyv(isolate(dataset()), input[["select_geo_column"]])
      #prediction_area$data <- dataset()[c(area_code, paste0("t", area_code)), c(input[["select_geo_column"]], input[["select_time_column"]], input[["select_measurements_column"]]), with=FALSE]
      if(input[["select_data_type"]] == "Poland"){
         area_code=paste0("t", area_code)
      }
      prediction_area$data <- isolate(dataset())[isolate(dataset())[[input[["select_geo_column"]]]]==area_code, c(input[["select_geo_column"]], input[["select_time_column"]], input[["select_measurements_column"]]), with=FALSE]
      prediction_area$data <- stats::na.omit(prediction_area$data)
      
      if(nrow(prediction_area$data) != 0){
        shinyjs::enable("to_prediction_button")
        output[["prediction_plot"]] <- renderPlotly({
            
            n<-nrow(prediction_area$data)
            vector_time <- prediction_area$data[[input[["select_time_column"]]]]
            vector_stat <-prediction_area$data[[input[["select_measurements_column"]]]]
            xaxis<-input[["select_time_column"]]
            yaxis<-input[["select_measurements_column"]]
            area_name<-sub(".*\\;","", input[["map_shape_click"]]$id)
            time_series_visualization(vector_time, vector_stat, area_name, n, h_to_pred=3, xaxis, yaxis)
            
        })  
      }
    }else{
      shinyjs::disable("to_prediction_button")
    }
  
  
  }) 


    
}
