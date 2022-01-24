#' Validating a given dataframe to check whether it is in UTF-8 encoding
#' 
#' @param data \code{data.frame} selected data for validaling
#' 
#' @return logical value, TRUE when the data ecoding is valid
#' 

encoding_check <- function(data){
  if(is.null(data)){
    return(FALSE)
  }
  
  char_columns<-as.list(data[, sapply(data, class) == 'character'])
  
  for(col in char_columns){
    if(!all(validUTF8(col))){
      return(FALSE)
    }
  }
  
  return(TRUE)
}





