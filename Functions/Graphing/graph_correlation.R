#' graph_correlation
#'
#' @param df dataframe
#' @param x_data data for x axis
#' @param y_data data for y axis
#' @param xlabel label for x axis
#' @param ylabel label for y axis
#'
#' @examples

graph_correlation = function(df, x_data, y_data, xlabel = "", ylabel = "", title = "") {
  
  # get rmse and mae
  rmse_s = rmse(x_data, y_data)
  mae_s = mae(x_data, y_data)
  
    # graph time series and add linear trendline 
    plot <- ggplot(df, aes(x = x_data, y = y_data)) +
      geom_point() +
      labs(
        title = sprintf("%s", title),
        x = sprintf("%s", xlabel),
        y = sprintf("%s", ylabel),
        subtitle = sprintf("RMSE: %s mm | MAE: %s mm", round(rmse_s,2), round(mae_s,2))
      ) +
    #  scale_x_continuous(expand = c(0,0)) +
     # scale_y_continuous(expand = c(0,0)) +
      theme_bw(base_size = 14)
    
  return(plot)
  
}