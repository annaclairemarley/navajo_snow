#' stream_xls_to_df
#'
#' @param path path to excel file
#' @param xls name of excel file
#' @param sheet_no which sheets on the excel workbook you'd like to process
#'
#' @return one dataframe of cleaned streamflow data with water years
#'
#' @examples stream_xls_to_df(path_stream, "Asaayi Daily Q's WY92-Current.xls", sheet_no = c(1:26))

stream_xls_to_df = function(path, xls, sheet_no){
  
  # get names of excel sheets
  list_sheets <- excel_sheets(paste0(path, xls))[sheet_no]
  
  # list of na strings that commonly occur in this dataset
  na_strings <- c("------", "-----", "----", "-----------------", "NO DATA", 
                  " N O   D A T A   T H I S   W A T E R   Y E A R", "------",
                  "---------", "------------")
  
  df = NULL # creat empty dataframe
  
  for (sheet_name in list_sheets) {
    
    print(sheet_name)
    
    # read individual excel sheets and clean
    readFile <- read.xls(paste0(path, xls), 
                         skip=3, sheet = sheet_name, header = TRUE, stringsAsFactors=FALSE) %>% 
      head(31) %>% 
      replace_with_na_all(condition = ~. %in% na_strings) %>%
      select(1:13) %>% 
      mutate(WY = sheet_name) %>% 
      mutate(WY = str_remove(WY, "WY")) %>% 
      mutate(WY = str_remove(WY, "_"))
    
    if (is.null(df)) { # don't merge the first one
      df = readFile
    } else {
      df = rbind(df, readFile) # bind the files together
    }
  }
  
  return(df)
}
