#' Moving Average Filtering Methods
#'
#' @description Internal functions for various moving average trend extraction methods.
#' These methods are optimized using the TTR package for performance and include
#' simple, exponential, adaptive, and hybrid moving averages.
#'
#' @name ma-filters
#' @keywords internal

#' Extract simple moving average trend
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

  # Use TTR's optimized SMA implementation (C code)
  ma_result <- TTR::SMA(as.numeric(ts_data), n = window)

  # Handle NAs at the beginning by using original values
  if (any(is.na(ma_result))) {
    na_count <- sum(is.na(ma_result))
    ma_result[1:na_count] <- as.numeric(ts_data)[1:na_count]
  }

  # Convert back to ts object
  trend <- stats::ts(
    ma_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend)
}

#' Extract EWMA trend
#' @noRd
.extract_ewma_trend <- function(ts_data, alpha, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing EWMA with alpha = {alpha}")
  }

  return(.ewma(ts_data, alpha))
}

#' Exponentially Weighted Moving Average
#' @noRd
.ewma <- function(ts_data, alpha = 0.1) {
  # Use TTR's optimized EMA implementation (C code)
  ema_result <- TTR::EMA(as.numeric(ts_data), n = NULL, ratio = alpha)

  # Handle first value (EMA starts with second observation)
  if (is.na(ema_result[1])) {
    ema_result[1] <- as.numeric(ts_data)[1]
  }

  trend_ts <- stats::ts(
    ema_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract ALMA trend
#' @noRd
.extract_alma_trend <- function(ts_data, window, offset, sigma, .quiet) {
  if (!.quiet) {
    cli::cli_inform(
      "Computing ALMA with window = {window}, offset = {offset}, sigma = {sigma}"
    )
  }

  return(.alma(ts_data, window, offset, sigma))
}

#' Adaptive Linear Moving Average (ALMA)
#' @noRd
.alma <- function(ts_data, window = 9, offset = 0.85, sigma = 6) {
  # Use TTR's optimized ALMA implementation (C code)
  alma_result <- TTR::ALMA(as.numeric(ts_data), n = window, offset = offset, sigma = sigma)

  # Handle NAs at the beginning
  if (any(is.na(alma_result))) {
    na_indices <- which(is.na(alma_result))
    alma_result[na_indices] <- as.numeric(ts_data)[na_indices]
  }

  trend_ts <- stats::ts(
    alma_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract DEMA trend
#' @noRd
.extract_dema_trend <- function(ts_data, period, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing DEMA with period = {period}")
  }

  return(.dema(ts_data, period))
}

#' Double Exponential Moving Average (DEMA)
#' @noRd
.dema <- function(ts_data, period = 14) {
  # Use TTR's optimized DEMA implementation (C code)
  dema_result <- TTR::DEMA(as.numeric(ts_data), n = period)

  # Handle NAs at the beginning
  if (any(is.na(dema_result))) {
    na_indices <- which(is.na(dema_result))
    dema_result[na_indices] <- as.numeric(ts_data)[na_indices]
  }

  trend_ts <- stats::ts(
    dema_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract HMA trend
#' @noRd
.extract_hma_trend <- function(ts_data, period, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing HMA with period = {period}")
  }

  return(.hma(ts_data, period))
}

#' Hull Moving Average (HMA)
#' @noRd
.hma <- function(ts_data, period = 14) {
  # Use TTR's optimized HMA implementation (C code)
  hma_result <- TTR::HMA(as.numeric(ts_data), n = period)

  # Handle NAs at the beginning
  if (any(is.na(hma_result))) {
    na_indices <- which(is.na(hma_result))
    hma_result[na_indices] <- as.numeric(ts_data)[na_indices]
  }

  trend_ts <- stats::ts(
    hma_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}