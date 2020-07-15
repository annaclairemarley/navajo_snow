#' stream_data_tidy
#'
#' @param df streamflow data frame after it has been through the stream_xls_to_df function
#'
#' @return converts the dataframe into tidy format
#'
#' @examples

stream_data_tidy = function(df) {

df_tidy <- df %>% 
  mutate(WY = ifelse(WY == "1999-2000", "1999", WY)) %>% 
  pivot_longer(c(OCT, NOV, DEC, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP), 
               names_to = "month", values_to = "discharge_cfps") %>% 
  mutate(wy_date = paste(month, Day, WY, sep = "-")) %>% #make water year dat
  mutate(wy_date = mdy(wy_date)) %>% 
  mutate(E_day = ifelse(str_detect(discharge_cfps, "E"), "E", NA)) %>% # keep track of days with E
  mutate(r_day = ifelse(str_detect(discharge_cfps, "r"), "r", NA)) %>% 
  mutate(discharge_cfps = str_remove_all(discharge_cfps, "[^0-9.]")) %>% 
  mutate(discharge_cfps = as.numeric(discharge_cfps)) %>% 
  mutate(WY = as.integer(WY)) %>% 
  mutate(dishcarge_cmps = conv_unit(discharge_cfps, "ft3_per_sec", "m3_per_sec"))

return(df_tidy)

}