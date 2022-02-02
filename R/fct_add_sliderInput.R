#' Creates a shiny sliderInput UI
#' 
#' @param vector_time selected time column
#' @param check_for_time_slider \code{character} value from radioButtons shiny object
#' 
#' @importFrom shinyjs disabled
#' 
#' @return a shiny sliderInput
#' 
add_sliderInput <- function(vector_time, check_for_time_slider){
   if(is.null(vector_time)) return(NULL)
   
   if(unique(nchar(as.character(vector_time))) == 4){
     if(check_for_time_slider == "one_value"){
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
     if(check_for_time_slider == "one_value"){
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