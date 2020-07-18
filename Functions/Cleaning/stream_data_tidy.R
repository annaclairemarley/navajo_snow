#' stream_data_tidy
#'
#' @param df streamflow data frame after it has been through the stream_xls_to_df function
#'
#' @return converts the dataframe into tidy format
#'
#' @examples

stream_data_tidy = function(df, gauge_name = "") {

df_tidy <- df %>% 
  mutate(WY = ifelse(WY == "1999-2000", "1999", WY)) %>% 
  pivot_longer(c(OCT, NOV, DEC, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP), 
               names_to = "month", values_to = "discharge_cfps") %>% 
  mutate(month_day = paste0(month, "-", Day)) %>% 
  mutate(year = ifelse(month %in% c("OCT", "NOV", "DEC"), as.integer(WY) - 1, WY)) %>% 
  mutate(date = mdy(paste0(month_day, "-", year))) %>% 
  mutate(wy_date = mdy(paste0(month_day, "-", WY))) %>% #make water year date
  mutate(E_day = ifelse(str_detect(discharge_cfps, "E"), "E", NA)) %>% # keep track of days with E
  mutate(r_day = ifelse(str_detect(discharge_cfps, "r"), "r", NA)) %>% 
  mutate(discharge_cfps = str_remove_all(discharge_cfps, "[^0-9.]")) %>% 
  mutate(discharge_cfps = as.numeric(discharge_cfps)) %>% 
  mutate(discharge_cmps = conv_unit(discharge_cfps, "ft3_per_sec", "m3_per_sec")) %>% 
  mutate(station = gauge_name) %>% 
  filter(!is.na(date))

return(df_tidy)
  
 

}