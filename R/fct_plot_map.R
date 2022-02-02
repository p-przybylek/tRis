#' Plotting leaflet map for selected dataset and columns.
#' 
#' @param df \code{data.table} selected dataset for visualization
#' @param map_type \code{character} type of geographical area - "Poland" or "World"
#' @param geo_column \code{character} name of column contains geographical data
#' @param date_column \code{character} name of column contains time data
#' @param measurements \code{character} name of column contains measurements
#' @param data_value \code{character} a value of date_column for select rows to visualization
#' 
#' @import data.table leaflet magrittr
#' @importFrom rnaturalearth ne_countries
#' @importFrom stats aggregate na.omit
#' 
#' @return map visualization
#' 
plot_map <- function(df, map_type, geo_column, date_column, measurements, data_value = NA){
  # check if the given columns are in the dataset
  if(isFALSE(all(c(geo_column, date_column, measurements) %in% colnames(df)))){
    return(NULL)
  }
  # setting data_value if NA
  if(length(data_value) == 1 && is.na(data_value)){
    data_value <- max(df[[date_column]])
  }
  
  if(map_type == "Poland"){
    vector_geo <- as.character(df[[geo_column]])
    if(any(unique(nchar(vector_geo)) %in% c(1,3))){
      vector_geo <- ifelse(nchar(vector_geo) == 1, paste0("0", vector_geo), vector_geo)
      vector_geo <- ifelse(nchar(vector_geo) == 3, ifelse(substr(vector_geo, 1,1) != "t", paste0("0", vector_geo), vector_geo), vector_geo)
      df[[geo_column]] <- vector_geo
    }
  }
  
  data.table::setkeyv(df, date_column)
  if(length(data_value) == 1){
    df_plot <- df[df[[date_column]] == data_value, c(date_column, geo_column, measurements), with=FALSE] 
  }else{
    df_sum <- function(x){ sum(x, na.rm = TRUE) }
    if(nchar(as.character(data_value[1])) == 10){
      date_range <- seq(as.Date(data_value[1]), as.Date(data_value[2]), by="days")
      df_plot <- df[df[[date_column]] %in% as.character(date_range), c(date_column, geo_column, measurements), with=FALSE]
      df_plot <- data.table::as.data.table(stats::aggregate(df_plot[[measurements]], list(df_plot[[geo_column]]), FUN=df_sum))
      colnames(df_plot) <- c(geo_column, measurements)
    }
    else{
      date_range <- seq(data_value[1], data_value[2])
      df_plot <- df[df[[date_column]] %in% date_range, c(date_column, geo_column, measurements), with=FALSE]
      df_plot <- data.table::as.data.table(stats::aggregate(df_plot[[measurements]], list(df_plot[[geo_column]]), FUN=df_sum))
      colnames(df_plot) <- c(geo_column, measurements) 
    }
  }
  # preparing data for visualization and visualize it
  if(map_type == "Poland"){
    vector_geo <- as.character(df_plot[[geo_column]])
    len <- unique(nchar(vector_geo))
    if(all(len %in% c(3,5))){ 
      df_plot[[geo_column]] <- substr(vector_geo, 2, nchar(vector_geo)) 
    }
    if(all(len %in% c(2,3))){
      poland <- readRDS(system.file("extdata", "gadm36_POL_1_sp.rds", package = "tRis"))
      poland@data$value <- df_plot[[measurements]][match(poland@data$CC_1, df_plot[[geo_column]])]
    }else{
      poland <- readRDS(system.file("extdata", "gadm36_POL_2_sp.rds", package = "tRis"))
      poland@data$value <- df_plot[[measurements]][match(poland@data$CC_2, df_plot[[geo_column]])]
    }
    pal <- leaflet::colorNumeric("viridis", poland@data$value, na.color="transparent", reverse = TRUE)
    map <- leaflet::leaflet(poland) %>% 
              leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
              leaflet::addPolygons(smoothFactor = 0.3,
                                fillColor = ~pal(value),
                                weight = 2,
                                opacity = 1,
                                highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                                color = "black",
                                dashArray = "3",
                                fillOpacity = 0.7,
                                label = ~paste0(NAME_2, ": ", formatC(value, big.mark = ",")),
                                layerId = ~paste0(CC_2,";", NAME_2)) %>% 
              leaflet::addLegend(pal = pal,
                                 values = ~value,
                                 title = measurements,
                                 opacity = 1,
                                 position = "bottomright") %>% 
              leaflet::fitBounds(lng1 = 14, lng2 = 24.5, lat1 = 50, lat2 = 55)
    return(map)
  }else{
    vector_geo <- as.character(df_plot[[geo_column]])
    len <- unique(nchar(vector_geo))
    world_countries <- rnaturalearth::ne_countries(scale = 50, returnclass="sp")
    if(all(len %in% c(2,8))){
      world_countries@data$value <- df_plot[[measurements]][match(world_countries@data$iso_a2, df_plot[[geo_column]])] 
      val <- "iso_a2"
    }else if(all(len %in% c(3,8)) && suppressWarnings(is.na(as.numeric(vector_geo)))){
      world_countries@data$value <- df_plot[[measurements]][match(world_countries@data$iso_a3, df_plot[[geo_column]])]
      val <- "iso_a3"
    }else{
      world_countries@data$value <- df_plot[[measurements]][match(world_countries@data$iso_n3, df_plot[[geo_column]])]
      val <- "iso_n3"
    }
    if(!all(is.finite(stats::na.omit(world_countries@data$value)))) return(NULL)
    pal <- leaflet::colorNumeric("viridis", world_countries@data$value, na.color="transparent", reverse = TRUE)
    map <- leaflet::leaflet(world_countries) %>% 
      leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
      leaflet::addPolygons(smoothFactor = 0.3,
                           fillColor = ~pal(value),
                           weight = 2,
                           opacity = 1,
                           highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                           color = "black",
                           dashArray = "3",
                           fillOpacity = 0.7,
                           label = ~paste0(name_long, ": ", formatC(value, big.mark = ",")),
                           layerId = ~paste0(world_countries@data[[val]], ";", name_long)) %>% 
      leaflet::addLegend(pal = pal,
                         values = ~value,
                         title = measurements,
                         opacity = 1,
                         position = "bottomright")
    return(map)
  }
}