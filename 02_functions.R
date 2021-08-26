#### Utility functions ####

#' Month Difference
#'
#' Function to get the number of months between two dates
#' @param start_date First date of the interval
#' @param end_date Second date of the interval
#' @returns Integer with the number of months
month_dif <- function(start_date, end_date) {
  x <- interval(start_date,
                end_date) %/% months(1)
  return(x)
}
