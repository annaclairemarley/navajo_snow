#' calculate_prism_snowfall
#'
#' returns a column in the dataframe that is PRISM accumulated snowfall
#'
#' @param prism_df dataframe that has prism precipitation and temperature
#' @param temp column that has temp
#' @param precip column that has precip

calculate_prism_snowfall = function(prism_df, temp, precip) {
  
  snow_df <- prism_df %>% 
    mutate(tmean = temp) %>% 
    mutate(precip_mm = precip) %>% 
    mutate(precip_type = ifelse(tmean <= 0, "snow", "rain")) %>% 
    mutate(month = month(Date)) %>% 
    filter(precip_type != is.na(precip_type)) %>% 
    mutate(prism_snow_mm = ifelse(precip_type == "snow", precip_mm, 0)) %>% 
    group_by(waterYear) %>% 
    mutate(prism_snow_mm_accum = cumsum(prism_snow_mm)) %>%  # accumulate prism precip
    mutate(prism_snow_mm_accum = ifelse(month(Date) %in% c(4,5,6,7,8,9,10), 0, 
                                        prism_snow_mm_accum))
  
  return(snow_df$prism_snow_mm_accum)
  
}