#' Plotting leaflet map for selected dataset and columns.
#' 
#' @param df \code{data.table} selected dataset for visualization
#' @param map_type \code{character} type of geographical area - "Poland" or "World"
#' @param geo_column \code{character} name of column contains geographical data
#' @param date_column \code{character} name of column contains time data
#' @param measurements \code{character} name of column contains measurements
#' @param data_value \code{character} a value of date_column for select rows to visualization
#' 
#' @import data.table sp leaflet magrittr
#' @return map visualization
#' 
plot_map <- function(df, map_type, geo_column, date_column, measurements, data_value = NA){
  if(is.na(data_value)){
    data_value <- max(df[[date_column]])
  }
  data.table::setkeyv(df, date_column)
  df_plot <- df[data_value, c(date_column, geo_column, measurements), with=FALSE]
  if(map_type == "Poland"){
    vector_geo <- as.character(df_plot[[geo_column]])
    len <- unique(nchar(vector_geo))
    if(all(len %in% c(3,5))){ 
      df_plot[[geo_column]] <- substr(vector_geo, 2, nchar(vector_geo)) 
    }
    poland_powiat <- readRDS(system.file("extdata", "gadm36_POL_2_sp.rds", package = "tRis"))
    poland_powiat@data$value <- df_plot[[measurements]][match(poland_powiat@data$CC_2, df_plot[[geo_column]])]
    pal <- leaflet::colorNumeric("viridis", NULL, na.color="transparent")
    map <- leaflet::leaflet(poland_powiat) %>% 
              leaflet::addProviderTiles("MapBox", options = providerTileOptions(
                      id = "mapbox.light",
                      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
              leaflet::addPolygons(smoothFactor = 0.3,
                                fillColor = ~pal(value),
                                weight = 2,
                                opacity = 1,
                                color = "black",
                                dashArray = "3",
                                fillOpacity = 0.7,
                                label = ~paste0(NAME_2, ": ", formatC(value, big.mark = ",")))
    return(map)
  }else{
    # ToDo
  }
}