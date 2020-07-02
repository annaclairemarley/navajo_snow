#' Gets the water week number for a given date (week starts on Sunday)
#'
#' @param date the date to get the water week number for. N.B. this has to be a Date object, not a string
#'
#' @return water week number for a given date (1-52)
#' @export
#'
#' @examples to_water_week(date)
#' @examples mutate(week_water_year = to_water_week(date))

to_water_week = function(date) {
  octStartWeek = epiweek(ymd(paste(year(date), "-10-01")))
  diff = 52 - octStartWeek
  return((epiweek(date) + diff) %% 52 + 1)
  
}