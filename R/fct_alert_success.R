#' Displays shinyalert success message
#' 
#' @param content message content
#' 
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs enable
#' 
#,

alert_success <- function(content){
  
  shinyalert::shinyalert(content,
                           type = "success",
                           confirmButtonText = "OK",
                           confirmButtonCol = "#a6a6a6")
}