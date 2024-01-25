#' Convert time series to tibble
#'
#' @param x A time series as a `ts` object
#' @param date_colname Optional name for the date column
#' @param value_colname Optional name for the value column
#'
#' @returns a `tibble`
#' @export
#' @examples
#' # example code
#' ts_to_df(AirPassengers)
#'
#' # Using a custom name for the value column
#' ts_to_df(AirPassengers, value_colname = "passengers")
ts_to_df <- function(x, date_colname = NULL, value_colname = NULL) {

  if (!stats::is.ts(x)) {
    stop("Object `x` is not a time series (`ts`) object.")
  }

  if (is.null(date_colname)) {
    date_colname <- "date"
  }

  if (is.null(value_colname)) {
    value_colname <- "value"
  }

  dat <- data.frame(
    d = zoo::as.Date.ts(x),
    v = zoo::coredata(x)
  )

  names(dat) <- c(date_colname, value_colname)
  dat <- tibble::as_tibble(dat)

  return(dat)

}
