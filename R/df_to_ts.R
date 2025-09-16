#' Convert a data.frame into a time series (ts)
#'
#' @description
#' Converts a series, stored in a data.frame or tibble, into a ts object.
#'
#' @param x A `data.frame`, `tibble` or `data.table`.
#' @param date_colname Name of the date column. Defaults to `'date'`. Must be of class `Date`.
#' @param value_colname Name of the value column. Defaults to `'value'`. Must be `numeric`.
#' @param frequency The frequency of the series. Can be a shortened string (e.g. "M" for monthly) or a number (e.g. 12).
#'
#' @return A `ts` object
#' @export
#' @examples
#' ibc <- df_to_ts(ibcbr, value_colname = "ibcbr", frequency = "M")
#' class(ibc)
#' plot(ibc)
df_to_ts <- function(
    x,
    date_colname = "date",
    value_colname = "value",
    frequency = 12
    ) {

  # Check inputs

  # Check column names

  # Check if column names are present in data.frame
  nm <- names(x)
  if (!any(date_colname %in% nm) | !any(value_colname %in% nm)) {
    stop("Selected column names are not present in the `data.frame`.")
  }

  #> Select columns
  xvalue <- x[[value_colname]]
  xdate <- x[[date_colname]]

  if (any(is.na(xvalue))) {
    warning("Missing values detected. To interpolate check ...")
  }

  if (!inherits(xdate, "Date")) {
    stop(glue::glue("Date column selected must be of type `Date` not {class(xdate)}"))
  }

  if (any(is.na(xdate))) {
    warning("Missings values detected in date column. Using first non-NA value as starting point.")
  }

  xyear <- lubridate::year(min(xdate, na.rm = TRUE))
  xmonth <- lubridate::month(min(xdate, na.rm = TRUE))

  #> Check frequency argument
  available_freqs <- data.frame(
    char = c("A", "S", "Q", "M", "W", "D"),
    num = c(1, 2, 4, 12, 52, 252)
  )

  if (is.character(frequency)) {
    if (!any(frequency %in% available_freqs$char)) {
      stop(glue::glue("Frequency must be one of {available_freqs$char}"))
    }

    xfreq <- subset(available_freqs, char == frequency)$num

  }

  if (is.numeric(frequency)) {
    if (!any(frequency %in% available_freqs$num)) {
      stop(glue::glue("Frequency must be one of {available_freqs$num}"))
    }

    if (!any(frequency %in% available_freqs$num)) {
      warning("Non-standard frequency chosen.")
    }

    xfreq <- frequency
  }

  # Return time series
  y <- stats::ts(xvalue, start = c(xyear, xmonth), frequency = xfreq)

  return(y)

}
#> Avoid "no visible binding for global variable ‘char’"
char <- NULL
