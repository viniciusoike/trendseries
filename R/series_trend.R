#' Add series trend
#'
#' Adds a column with the trend of a series to a `data.frame` type object.
#'
#' This function extracts the trend of a time series and returns it as an additional
#' column in the original `data.frame`. There are three options
#'
#' @inheritParams df_to_ts
#' @param trend Method to extract trend from series. Must be one of `hp`, `ma`, `poly`, `stl`, or `all` (default).
#' @param poly_k Order of the polynomial trend.
#' @param window_ma Size of the MA filter window (e.g. 5 for 5-period MA).
#' @param window_stl Size of the seasonal window parameter in STL. Equivalent to `s.window`.
#' @param lambda_hp Value of the smoothing parameter for the HP Filter. Defaults to 1600.
#' @param ... Additional arguments sent to trending methods.
#'
#' @details The `trend` argument can be one of: `hp`, `ma`, `poly`, `stl`,
#' or `all`
#'
#' @returns A `data.frame` containing an extra column named `trend_*`.
#' @importFrom hpfilter hp2
#' @importFrom zoo coredata as.Date.ts
#' @importFrom tibble as_tibble
#' @export
#' @examples
#'
#' # All trends available
#' add_trend(
#'   gdp_construction,
#'   value_colname = "gdp_construction",
#'   trend = "all",
#'   frequency = 4
#' )
#'
#' # Automatically selects a 2x4 MA window
#' gdp_ma <- add_trend(
#'   gdp_construction,
#'   value_colname = "gdp_construction",
#'   trend = "ma",
#'   frequency = 4)
#'
#' # Select a different window
#' gdp_ma <- add_trend(
#'   gdp_construction,
#'   value_colname = "gdp_construction",
#'   trend = "ma",
#'   frequency = 4,
#'   window_ma = 11
#' )

add_trend <- function(
  x,
  date_colname = "date",
  value_colname = "value",
  frequency = 12,
  trend = "all",
  poly_k = 1,
  window_ma = NULL,
  window_stl = 13,
  lambda_hp = 1600,
  ...
) {
  y <- df_to_ts(x, date_colname, value_colname, frequency)

  if (length(y) < 3 * stats::frequency(y)) {
    warning("There are too few observations to extract a trend.")
    return(tibble::as_tibble(x))
  }

  if (length(trend) == 1 && trend == "all") {
    trend <- c("ma", "stl", "hp", "poly")
  }

  # Polynomial trend
  if (any("poly" %in% trend)) {
    #> Extract time sequence
    tt <- stats::time(y)
    #> Linear model, polynomial regression
    model_lm <- stats::lm(y ~ stats::poly(tt, degree = poly_k, raw = TRUE))
    #> Convert polynomial trend to ts
    trend.poly <- stats::ts(
      data = stats::fitted(model_lm),
      start = stats::start(y),
      frequency = stats::frequency(y)
    )
  }

  # Moving average trend
  if (any("ma" %in% trend)) {
    # Define the window
    if (is.null(window_ma)) {
      fma <- stats::frequency(y)
    } else {
      fma <- window_ma
    }

    #> Compute a centered-MA trend
    trend.ma <- stats::filter(
      y,
      filter = rep(1 / fma, fma),
      method = "convolution"
    )

    #> Check if frequency is odd (make it symmetric)
    if (fma %% 2 == 0) {
      trend.ma <- stats::filter(trend.ma, filter = c(1 / 2, 1 / 2))
    }

    msg_ma <- ifelse(fma %% 2 == 0, 2, 1)

    message(glue::glue("Computed a {msg_ma}x{fma} MA-trend."))
  }

  if (any("stl" %in% trend)) {
    #> STL trend
    trend.stl <- stats::stl(y, s.window = window_stl, ...)$time.series[,
      "trend"
    ]
    message(glue::glue(
      "Computed STL trend using seasonal window = {window_stl}."
    ))
  }

  if (any("hp" %in% trend)) {
    #> Hodrick-Prescott Filter trend
    data_matrix <- matrix(x[[value_colname]], ncol = 1)
    hp_result <- hpfilter::hp2(data_matrix, lambda = lambda_hp)
    #> Convert to ts
    trend.hp <- stats::ts(
      data = hp_result[, 1],
      start = stats::start(y),
      frequency = stats::frequency(y)
    )
    message(glue::glue("Computed HP filter using lambda = {lambda_hp}"))
  }

  #> Get all existing objects names trend.
  sel_trends <- ls(pattern = "^trend\\.")

  if (length(sel_trends) == 1) {
    #> Convert to tibble
    trend <- ts_to_df(get(sel_trends))
  } else {
    #> Intersect all series
    ts_trend <- Reduce(stats::ts.intersect, mget(sel_trends))
    #> Convert to data.frame
    trend <- data.frame(
      date = zoo::as.Date.ts(ts_trend),
      zoo::coredata(ts_trend)
    )
  }
  #> Use trends as names for columns
  names(trend)[-1] <- gsub("\\.", "_", sel_trends)

  out <- merge(x, trend, by = date_colname, all.x = TRUE)
  out <- tibble::as_tibble(out)

  return(out)
}
