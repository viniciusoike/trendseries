#' Extract trends from time series objects
#'
#' @description
#' Extract trend components from time series objects using various econometric methods.
#' Designed for monthly and quarterly economic data analysis. Returns trend components
#' as time series objects or a list of time series.
#'
#' @param ts_data A time series object (`ts`, `xts`, or `zoo`)
#' @param methods Character vector of trend methods. Options: `"hp"`, `"bk"`, `"cf"`,
#'   `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`. Default is `"hp"`.
#' @param hp_lambda Smoothing parameter for HP filter. If NULL, uses economic defaults.
#' @param bk_low,bk_high Lower and upper bounds for Baxter-King filter (in periods).
#' @param cf_low,cf_high Lower and upper bounds for Christiano-Fitzgerald filter (in periods).
#' @param ma_window Window size for moving average. If NULL, uses frequency.
#' @param stl_s_window Seasonal window for STL. If NULL, uses "periodic".
#' @param loess_span Span parameter for loess smoother.
#' @param spline_spar Smoothing parameter for smooth.spline.
#' @param poly_degree Degree for polynomial trend.
#' @param .quiet If TRUE, suppress informational messages.
#'
#' @return If single method, returns a `ts` object. If multiple methods, returns
#'   a named list of `ts` objects.
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom stats is.ts frequency start time ts fitted lm poly loess smooth.spline stl filter var
#' @importFrom hpfilter hp2
#' @importFrom tsbox ts_ts
#' @importFrom zoo as.Date.ts coredata
#' @importFrom lubridate year month quarter
#'
#' @details
#' This function focuses on monthly (frequency = 12) and quarterly (frequency = 4)
#' economic data. It uses established econometric methods with appropriate defaults:
#'
#' - **HP Filter**: lambda=1600 (quarterly), lambda=14400 (monthly)
#' - **Baxter-King**: Bandpass filter for business cycles (6-32 quarters default)
#' - **Christiano-Fitzgerald**: Asymmetric bandpass filter
#' - **Moving Average**: Centered, frequency-appropriate windows
#' - **STL**: Seasonal-trend decomposition
#' - **Loess**: Local polynomial regression
#' - **Spline**: Smoothing splines
#' - **Polynomial**: Linear/polynomial trends
#'
#' @examples
#' # Single method
#' hp_trend <- extract_trends(AirPassengers, methods = "hp")
#'
#' # Multiple methods
#' all_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("hp", "bk", "ma")
#' )
#'
#' # Custom parameters
#' bk_trend <- extract_trends(
#'   AirPassengers,
#'   methods = "bk",
#'   bk_low = 8, bk_high = 40
#' )
#'
#' @export
extract_trends <- function(ts_data,
                          methods = "hp",
                          hp_lambda = NULL,
                          bk_low = 6, bk_high = 32,
                          cf_low = 6, cf_high = 32,
                          ma_window = NULL,
                          stl_s_window = NULL,
                          loess_span = 0.75,
                          spline_spar = NULL,
                          poly_degree = 1,
                          .quiet = FALSE) {

  # Convert to ts object using tsbox if needed
  if (!stats::is.ts(ts_data)) {
    ts_data <- tsbox::ts_ts(ts_data)
  }

  # Validate frequency
  freq <- stats::frequency(ts_data)
  if (!freq %in% c(4, 12)) {
    cli::cli_abort(
      "Only monthly (12) and quarterly (4) frequencies are supported.
       Time series frequency: {freq}"
    )
  }

  # Check minimum observations
  min_obs <- 3 * freq
  if (length(ts_data) < min_obs) {
    cli::cli_warn(
      "Series has {length(ts_data)} observations.
       Minimum {min_obs} recommended for reliable trend extraction."
    )
  }

  # Set defaults based on frequency
  if (is.null(hp_lambda)) {
    hp_lambda <- if (freq == 4) 1600 else 14400
  }

  if (is.null(ma_window)) {
    ma_window <- freq
  }

  if (is.null(stl_s_window)) {
    stl_s_window <- "periodic"
  }

  # Validate methods
  valid_methods <- c("hp", "bk", "cf", "ma", "stl", "loess", "spline", "poly")
  invalid_methods <- setdiff(methods, valid_methods)
  if (length(invalid_methods) > 0) {
    cli::cli_abort(
      "Invalid methods: {.val {invalid_methods}}.
       Valid options: {.val {valid_methods}}"
    )
  }

  # Extract trends
  trends <- list()

  for (method in methods) {
    trend <- switch(method,
      "hp" = .extract_hp_trend(ts_data, hp_lambda, .quiet),
      "bk" = .extract_bk_trend(ts_data, bk_low, bk_high, .quiet),
      "cf" = .extract_cf_trend(ts_data, cf_low, cf_high, .quiet),
      "ma" = .extract_ma_trend(ts_data, ma_window, .quiet),
      "stl" = .extract_stl_trend(ts_data, stl_s_window, .quiet),
      "loess" = .extract_loess_trend(ts_data, loess_span, .quiet),
      "spline" = .extract_spline_trend(ts_data, spline_spar, .quiet),
      "poly" = .extract_poly_trend(ts_data, poly_degree, .quiet)
    )

    trends[[method]] <- trend
  }

  # Return single ts if one method, list if multiple
  if (length(methods) == 1) {
    return(trends[[1]])
  } else {
    return(trends)
  }
}

# Individual trend extraction methods

#' @noRd
.extract_hp_trend <- function(ts_data, lambda, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing HP filter with lambda = {lambda}")
  }

  # Use hpfilter package
  # Convert to matrix as required by hp2
  data_matrix <- matrix(as.numeric(ts_data), ncol = 1)
  hp_result <- hpfilter::hp2(data_matrix, lambda = lambda)
  trend <- stats::ts(
    hp_result[, 1],  # hp2 returns data.frame with single column
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend)
}

#' @noRd
.extract_bk_trend <- function(ts_data, pl, pu, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing Baxter-King filter with bands [{pl}, {pu}]")
  }

  # Check if mFilter is available
  if (!requireNamespace("mFilter", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg mFilter} is required for Baxter-King filter")
  }

  bk_result <- mFilter::bkfilter(ts_data, pl = pl, pu = pu)
  return(bk_result$trend)
}

#' @noRd
.extract_cf_trend <- function(ts_data, pl, pu, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing Christiano-Fitzgerald filter with bands [{pl}, {pu}]")
  }

  if (!requireNamespace("mFilter", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg mFilter} is required for Christiano-Fitzgerald filter")
  }

  cf_result <- mFilter::cffilter(ts_data, pl = pl, pu = pu)
  return(cf_result$trend)
}

#' @noRd
.extract_ma_trend <- function(ts_data, window, .quiet) {
  freq <- stats::frequency(ts_data)

  # Determine message based on window and frequency
  if (window == freq && freq %% 2 == 0) {
    msg <- "2x{window}"
  } else {
    msg <- "{window}"
  }

  if (!.quiet) {
    cli::cli_inform("Computing {msg}-period moving average")
  }

  # Centered moving average
  trend <- stats::filter(ts_data, filter = rep(1/window, window), method = "convolution")

  # For even frequencies, apply additional 2-point average
  if (window %% 2 == 0) {
    trend <- stats::filter(trend, filter = c(0.5, 0.5))
  }

  return(trend)
}

#' @noRd
.extract_stl_trend <- function(ts_data, s_window, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing STL trend with s.window = {s_window}")
  }

  # Check if series has enough seasonality for STL
  freq <- stats::frequency(ts_data)
  if (freq == 1) {
    cli::cli_warn("STL not applicable for non-seasonal data. Using HP filter instead.")
    return(.extract_hp_trend(ts_data, lambda = 1600, .quiet = TRUE))
  }

  stl_result <- stats::stl(ts_data, s.window = s_window)
  return(stl_result$time.series[, "trend"])
}

#' @noRd
.extract_loess_trend <- function(ts_data, span, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing loess trend with span = {span}")
  }

  # Create time index
  time_index <- as.numeric(stats::time(ts_data))
  values <- as.numeric(ts_data)

  # Fit loess
  loess_fit <- stats::loess(values ~ time_index, span = span)
  trend_values <- stats::fitted(loess_fit)

  # Convert back to ts
  trend <- stats::ts(
    trend_values,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend)
}

#' @noRd
.extract_spline_trend <- function(ts_data, spar, .quiet) {
  if (!.quiet) {
    msg <- if (is.null(spar)) "automatic smoothing" else "spar = {spar}"
    cli::cli_inform("Computing spline trend with {msg}")
  }

  # Create time index
  time_index <- as.numeric(stats::time(ts_data))
  values <- as.numeric(ts_data)

  # Fit smoothing spline
  if (is.null(spar)) {
    spline_fit <- stats::smooth.spline(time_index, values)
  } else {
    spline_fit <- stats::smooth.spline(time_index, values, spar = spar)
  }

  trend_values <- stats::fitted(spline_fit)

  # Convert back to ts
  trend <- stats::ts(
    trend_values,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend)
}

#' @noRd
.extract_poly_trend <- function(ts_data, degree, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing polynomial trend with degree = {degree}")
  }

  # Create time index
  time_index <- as.numeric(stats::time(ts_data))
  values <- as.numeric(ts_data)

  # Fit polynomial
  poly_fit <- stats::lm(values ~ stats::poly(time_index, degree = degree, raw = TRUE))
  trend_values <- stats::fitted(poly_fit)

  # Convert back to ts
  trend <- stats::ts(
    trend_values,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend)
}