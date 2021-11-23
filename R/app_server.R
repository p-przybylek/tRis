#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny shinydashboard
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs onclick enable disable
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
  output$select_file = renderUI({ # render select file
    
    # validating filetypes by extensions
    # extension_list<-ifelse(input[["select_separator"]]=="csv",
    #                        c("text/csv",'csv'),
    #                        ifelse(
    #                          input[["select_separator"]]=="txt",
    #                          c("text/plain",".txt"),
    #                          ifelse(
    #                            input[["select_separator"]]=="xlsx",
    #                            c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx"),
    #                            c("csv"))))
    
    
    extension_list<-c("text/csv",'csv',"text/plain",".txt","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx")
    fileInput("select_file", NULL,
              accept = extension_list,
              buttonLabel = "Upload",
              placeholder = "Please choose a file...")
    
  })
  observeEvent({input[["return_to_start_button1"]]},{ # reload fileInput when user returns to start panel
    output$select_file = renderUI({

      extension_list<-c("text/csv",'csv',"text/plain",".txt","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xlsx")
      fileInput("select_file", NULL,
                accept = extension_list,
                buttonLabel = "Upload",
                placeholder = "Please choose a file...")
      
    })
    
  })

  observeEvent( # when filetype is xlsx, separator options are disabled
    {
      input[["select_filetype"]]
    }, 
    { 
      if(input[["select_filetype"]]=='xlsx'){
        shinyjs::disable("select_separator")
      }else{
        shinyjs::enable("select_separator")
      }
    })
  

  
  
  observeEvent( # when filetype and separator aren't chosen, file select is disabled
    {
      input[["select_filetype"]] 
      input[["select_separator"]]}, 
    { 
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
  
  
  # check the values is select_file - for debugging purposes
  # output[["file_summary"]]<-renderTable({
  #   input[['select_file']]
  # })

  
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
      if(is.null(data_file)){return()}
      
      ### check filetype and alert if invalid
      
      filetype<-input[['select_filetype']]
      if(filetype=='csv'){
        
          if( data_file$type=="text/csv"){
            user_dataset<-utils::read.csv(file=data_file$datapath,
                                   sep=input[['select_separator']],
                                   header=TRUE)
            # display a confirmation
            shinyalert::shinyalert("Data loaded successfully",
                                   type = "success",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            shinyjs::enable("to_view_data_button1")
            shinyjs::enable("to_visualize_data_button1")
          }
          else{
            user_dataset<-NULL
            # display error
            shinyalert::shinyalert("Invalid extension, choose a .csv file or a valid extension type for your file",
                                   type = "error",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            shinyjs::disable("to_view_data_button1")
            shinyjs::disable("to_visualize_data_button1")

          }

        }
        else if(filetype=='txt'){

          if( data_file$type=="text/plain"){
            user_dataset<-utils::read.table(file=data_file$datapath,
                                     sep=input[['select_separator']],
                                     header=TRUE)
            # display a confirmation
            shinyalert::shinyalert("Data loaded successfully",
                                   type = "success",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            shinyjs::enable("to_view_data_button1")
            shinyjs::enable("to_visualize_data_button1")
          }
          else{
            user_dataset<-NULL
            # display error
            shinyalert::shinyalert("Invalid extension, choose a .txt file or a valid extension type for your file",
                                   type = "error",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            shinyjs::disable("to_view_data_button1")
            shinyjs::disable("to_visualize_data_button1")

          }
          
          
        }
        else if(filetype=='xlsx'){

          if( data_file$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
            user_dataset<-openxlsx::read.xlsx(data_file$datapath)
            # display a confirmation
            shinyalert::shinyalert("Data loaded successfully",
                                   type = "success",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            shinyjs::enable("to_view_data_button1")
            shinyjs::enable("to_visualize_data_button1")
          }
          else{
            user_dataset<-NULL
            # display error
            shinyalert::shinyalert("Invalid extension, choose a .xlsx file or a valid extension type for your file",
                                   type = "error",
                                   confirmButtonText = "OK",
                                   confirmButtonCol = "#a6a6a6")
            shinyjs::disable("to_view_data_button1")
            shinyjs::disable("to_visualize_data_button1")

          }
          
        }
      else{
        user_dataset<-NULL
        # display error
        shinyalert::shinyalert("Invalid extension type",
                               type = "error",
                               confirmButtonText = "OK",
                               confirmButtonCol = "#a6a6a6")
        shinyjs::disable("to_view_data_button1")
        shinyjs::disable("to_visualize_data_button1")
      }
        
        return(user_dataset)
        
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
  
  ### the data visualization and prediction interface
 
  shinyjs::onclick("return_to_visualization_data_button", { # back to visualization data interface
    new_interface <- switch(input[["interfaces"]],
                            "prediction" = "visualization",
                            "visualization" = "prediction")
    shinydashboard::updateTabItems(session, "interfaces", new_interface)
  })
  
}
