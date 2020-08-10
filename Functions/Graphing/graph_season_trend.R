#' graph_season_trend
#'
#' @param df dataframe of climate trend by year
#' @param climate_data the y axis column name, make sure you do df$column_name
#' @param ylabel what you want to call the y axis on the graph
#'
#' @return graph of time series with a linear model fitted
#' @export
#'
#' @examples graph_season_trend(spring_av, climate_data = spring_av$precip_av, ylabel = "Average Spring Precipitation (mm)")
graph_season_trend = function(df, climate_data, year, title = "", ylabel = "", xlabel = "", 
                              graph_type = "point_line"){

  
  # get linear trend equation
  lm = lm(climate_data ~ year)
  intercept = coef(lm)[1]
  slope = coef(lm)[2]
  pvalue = summary(lm)$coefficients[2,4]
  
  
  if (graph_type == "point_line") {
    # graph time series and add linear trendline 
    plot <- ggplot(df, aes(x = year, y = climate_data)) +
      geom_line() +
      geom_point() +
      labs(
        title = sprintf("%s", title),
        x = sprintf("%s", xlabel),
        y = sprintf("%s", ylabel),
        subtitle = sprintf("Slope: %s, p-value: %s", 
                           round(slope, 2), formatC(pvalue, format = "e", digits = 2))
      ) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(breaks = seq(1981,2021, by = 3), expand = c(0,0)) +
      theme_bw(base_size = 15)
    
  } else {
    # graph time series and add linear trendline 
    plot <- ggplot(df, aes(x = year, y = climate_data)) +
      geom_point() +
      labs(
        title = sprintf("%s", title),
        x = sprintf("%s", xlabel),
        y = sprintf("%s", ylabel),
        subtitle = sprintf("Slope: %s, p-value: %s", 
                           round(slope, 2), formatC(pvalue, format = "e", digits = 2))
      ) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(breaks = seq(1981,2021, by = 3), expand = c(0,0)) +
      theme_bw(base_size = 15)
  }
  
  
  return(plot)
  
}