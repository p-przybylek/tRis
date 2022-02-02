#' Creates a shiny fileInput UI
#' 
#' @param extention_list of allowed file extentions
#' @param disabled logical value, whether the input is disabled
#' 
#' @importFrom shinyjs disabled
#' 
#' @return a shiny fileInput
#' 
#' 

add_fileInput <- function(extention_list, disabled){
  

  if(disabled){
    
      shinyjs::disabled(
        fileInput("select_file", NULL,
                  accept = extention_list,
                  buttonLabel = "Upload",
                  placeholder = "Please choose a file...")
      )
      
    }
  else{
    fileInput("select_file", NULL,
              accept = extention_list,
              buttonLabel = "Upload",
              placeholder = "Please choose a file...")
    
  }
}