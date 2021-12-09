#' Validating a given date/time column.
#' 
#' @param vector_time \code{character} selected column for validaling
#' 
#' @return logical value
#' 
time_column_check <- function(vector_time){
  len <- unique(nchar(vector_time))
  val <- FALSE
  if(length(len) != 1 || (len != 4 && len != 10)){
    val <- TRUE
  }else{
    if(len == 10){
      check_type <- (unique(nchar(sub("\\..*", "", vector_time))) != 4)
      if(length(check_type) != 1){
        val <- TRUE
      }else if(check_type && any(is.na(as.Date(vector_time, format="%Y-%m-%d")))){
        val <- TRUE
      }else if(!check_type && any(is.na(as.Date(vector_time, format="%Y.%m.%d")))){
        val <- TRUE
      }
    }else{
      if(is.na(all(as.integer(vector_time))) || isFALSE(all(as.integer(vector_time) >= 1000)) || isFALSE(all(as.integer(vector_time) <= as.integer(format(Sys.Date(), "%Y"))))){
        val <- TRUE
      }
    }
  }
  return(val)
}

#' Validating a given geographic column.
#' 
#' @param vector_geo \code{character or numeric} selected column for validaling
#' @param data_type \code{character} type of geographical area - "Poland" or "World"
#' 
#' @import maps
#' 
#' @return logical value
#' 
geo_column_check <- function(vector_geo, data_type){
  len <- unique(nchar(vector_geo))
  val <- FALSE
  if(data_type == "Poland"){ # properly format of a column is TERYT
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
  }else if(data_type == "World"){ # properly format of a column is ISO 3166-1 code alpha-1 or aplha-2
    if((length(len) != 1 && length(len) != 2) || !all(len %in% c(2,3,8))){
      val <- TRUE
    }else{
      if(all(len %in% c(2,8)) && !all(vector_geo %in% c("OWID_WRL", maps::iso3166$a2))){
        val <- TRUE
      }else if(all(len %in% c(3,8))){
        if(all(is.na(as.numeric(vector_geo))) && !all(vector_geo %in% c("OWID_WRL", maps::iso3166$a3))){
          val <- TRUE
        }else if(!all(is.na(as.numeric(vector_geo)))){
          val <- TRUE
        }
      }else if(len == 8 && (length(len) == 1)){
        val <- TRUE
      }
    }
  }
  return(val)
}