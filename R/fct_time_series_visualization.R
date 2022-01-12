#' Creates a file input UI
#' 
#' @param vector_time
#' @param vector_stat
#' @param area_name
#' @param n
#' @param h_to_pred number of periods to predict
#' 
#' @importFrom forecast forecast auto.arima
#' @importFrom plotly renderPlotly plot_ly add_trace
#' @importFrom stats ts na.omit


time_series_visualization<-function(vector_time, vector_stat, area_name, n, h_to_pred=3, xaxis, yaxis){
  
  # create time series 
  if(unique(nchar(as.character(vector_time))) == 4){
    
    # RRRR format
    series <- ts(vector_stat, start=vector_time[1], end=vector_time[n])
  }else{
    
    # RRRR-MM-DD or RRRR.MM.DD format
    format <- ifelse(substr(vector_time[1],5,5) == "-", "%Y-%m-%d", "%Y.%m.%d")
    vector_time<-as.Date(vector_time)
    series <- ts(vector_stat, start=vector_time[1], end=vector_time[n]+1)
  }
  model <- forecast::auto.arima(series,
                                stationary = FALSE,
                                seasonal=TRUE)
  forecast_output <- forecast::forecast(series, h=h_to_pred, model=model)
  
  ## check datatype in arima and adjust forecast
  if(class(vector_time)=="integer"){
    
    forecast_output$mean<-as.integer(forecast_output$mean)
    forecast_output$upper<-as.integer(forecast_output$upper)
    forecast_output$lower<-as.integer(forecast_output$lower)
  }else{
    d<-2
    forecast_output$mean<-round(forecast_output$mean, d)
    forecast_output$upper<-round(forecast_output$upper, d)
    forecast_output$lower<-round(forecast_output$lower, d)
  }
  
  
  dates<-c(vector_time, vector_time[n]+1:h_to_pred)
  observations<-c(forecast_output$x, rep(NA, 3))
  mean<-c(rep(NA, n-1), forecast_output$x[n], forecast_output$mean[1:h_to_pred])
  high80<-c(rep(NA, n-1),forecast_output$x[n],  forecast_output$upper[1:h_to_pred])
  low80<-c(rep(NA, n-1),forecast_output$x[n], forecast_output$lower[1:h_to_pred])
  
  df<-data.frame(date=dates, observations=observations, mean=mean, high80=high80, low80=low80)
  
  vline <- function(x = 0) {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = "#B9C3D1")
    )
  }
  
  plotly::plot_ly(df, 
                  type = "scatter", 
                  mode = "lines", 
                  colors=c("#A1CDBC", "#A997DF"))%>%
    plotly::add_trace(x = ~date, y = ~observations, line=list(color="#A1CDBC"),  name="Observations")%>%
    plotly::add_trace(x = ~date, y = ~mean, line=list(color="#A997DF", dash="dash"),  name="Mean")%>%
    plotly::add_trace(x = ~date,  y= ~high80, type = 'scatter', mode = 'lines',
                      fill = 'tonexty', fillcolor="#C6BEDF",
                      line = list(color = 'transparent'), name="Upper bound")%>%
    plotly::add_trace(x = ~date,  y= ~low80, type = 'scatter', mode = 'lines',
                      fill = 'tonexty', fillcolor="#C6BEDF",
                      line = list(color = 'transparent'), name="Lowe bound")%>%
    plotly::add_trace(x = ~date, y = ~mean, line=list(color="#A997DF", dash="dash"),  name="Mean")%>%
    plotly::layout(showlegend = FALSE, 
                   shapes = list(vline(vector_time[n])),
                   title=paste0("Time series for ",area_name, ", prediction for 3 periods"),
                   xaxis = list(rangeslider = list(visible = T),
                                zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff',
                                title = xaxis),
                   yaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff',
                                title = yaxis),
                   plot_bgcolor='#e5ecf6')
  
}


