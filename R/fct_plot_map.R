#' Plotting leaflet map for selected dataset and columns.
#' 
#' @param df \code{data.table} selected dataset for visualization
#' @param map_type \code{character} type of geographical area - "Poland" or "World"
#' @param geo_column \code{character} name of column contains geographical data
#' @param date_column \code{character} name of column contains time data
#' @param measurements \code{character} name of column contains measurements
#' @param data_value \code{character} a value of date_column for select rows to visualization
#' 
#' @import data.table sp leaflet magrittr rnaturalearth
#' @importFrom stats aggregate
#' @return map visualization
#' 
plot_map <- function(df, map_type, geo_column, date_column, measurements, data_value = NA){
  if(length(data_value) == 1 && is.na(data_value)){
    data_value <- max(df[[date_column]])
  }
  data.table::setkeyv(df, date_column)
  if(length(data_value) == 1){
    df_plot <- df[df[[date_column]] == data_value, c(date_column, geo_column, measurements), with=FALSE] 
  }else{
    if(nchar(as.character(data_value[1])) == 10){
      date_range <- seq(as.Date(data_value[1]), as.Date(data_value[2]), by="days")
      df_plot <- df[df[[date_column]] %in% as.character(date_range), c(date_column, geo_column, measurements), with=FALSE]
      #df_plot[, list(sum=sum(measurements)), by=geo_column]
      df_plot <- as.data.table(stats::aggregate(df_plot[[measurements]], list(df_plot[[geo_column]]), FUN=sum))
      colnames(df_plot) <- c(geo_column, measurements)
    }
    else{
      date_range <- seq(data_value[1], data_value[2])
      df_plot <- df[df[[date_column]] %in% date_range, c(date_column, geo_column, measurements), with=FALSE]
      df_plot <- as.data.table(stats::aggregate(df_plot[[measurements]], list(df_plot[[geo_column]]), FUN=sum))
      colnames(df_plot) <- c(geo_column, measurements) 
    }
  }
  if(map_type == "Poland"){
    vector_geo <- as.character(df_plot[[geo_column]])
    len <- unique(nchar(vector_geo))
    if(all(len %in% c(3,5))){ 
      df_plot[[geo_column]] <- substr(vector_geo, 2, nchar(vector_geo)) 
    }
    poland_powiat <- readRDS(system.file("extdata", "gadm36_POL_2_sp.rds", package = "tRis"))
    poland_powiat@data$value <- df_plot[[measurements]][match(poland_powiat@data$CC_2, df_plot[[geo_column]])]
    pal <- leaflet::colorNumeric("plasma", NULL, na.color="transparent")
    map <- leaflet::leaflet(poland_powiat) %>% 
              leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
              leaflet::addPolygons(smoothFactor = 0.3,
                                fillColor = ~pal(value),
                                weight = 2,
                                opacity = 1,
                                color = "black",
                                dashArray = "3",
                                fillOpacity = 0.7,
                                label = ~paste0(NAME_2, ": ", formatC(value, big.mark = ",")),
                                layerId = ~CC_2) %>% 
              leaflet::addLegend(pal = pal,
                                 values = ~value,
                                 opacity = 1,
                                 position = "topright")
    return(map)
  }else{
    vector_geo <- as.character(df_plot[[geo_column]])
    len <- unique(nchar(vector_geo))
    world_countries <- rnaturalearth::ne_countries(returnclass='sp')
    if(all(len %in% c(2,8))){
      world_countries@data$value <- df_plot[[measurements]][match(world_countries@data$iso_a2, df_plot[[geo_column]])] 
      val <- "iso_a2"
    }else if(all(len %in% c(3,8)) && is.na(as.numeric(vector_geo))){
      world_countries@data$value <- df_plot[[measurements]][match(world_countries@data$iso_a3, df_plot[[geo_column]])]
      val <- "iso_a3"
    }else{
      world_countries@data$value <- df_plot[[measurements]][match(world_countries@data$iso_n3, df_plot[[geo_column]])]
      val <- "iso_n3"
    }
    pal <- leaflet::colorNumeric("plasma", NULL, na.color="transparent")
    map <- leaflet::leaflet(world_countries) %>% 
      leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
      leaflet::addPolygons(smoothFactor = 0.3,
                           fillColor = ~pal(value),
                           weight = 2,
                           opacity = 1,
                           color = "black",
                           dashArray = "3",
                           fillOpacity = 0.7,
                           label = ~paste0(name_long, ": ", formatC(value, big.mark = ",")),
                           layerId = ~world_countries@data[[val]]) %>% 
      leaflet::addLegend(pal = pal,
                         values = ~value,
                         opacity = 1,
                         position = "topright")
    return(map)
  }
}