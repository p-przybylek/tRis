#' Displays shinyalert error message
#' 
#' @param content message content
#' 
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs disable
#' 
#' 

alert_error <- function(title, content){
  
  shinyalert::shinyalert(title,
                         content,
                         type = "error",
                         confirmButtonText = "OK",
                         confirmButtonCol = "#a6a6a6")
}