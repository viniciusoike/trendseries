#' Moving Average Filtering Methods
#'
#' @description Internal functions for various moving average trend extraction methods.
#' These methods are optimized using the TTR package for performance and include
#' simple, exponential, adaptive, and hybrid moving averages.
#'
#' @details
#' All moving average functions use TTR's C-optimized implementations for speed.
#' NAs are preserved at the beginning of the series as expected for moving averages.
#'
#' Parameter notes:
#' - **SMA**: window parameter specifies the number of periods
#' - **EWMA**: alpha parameter (0 < alpha < 1) controls smoothing strength
#' - **ALMA**: Arnaud Legoux MA with Gaussian weighting
#' - **DEMA**: Double exponential MA for reduced lag
#' - **HMA**: Hull MA combines WMAs for responsiveness
#'
#' @name ma-filters
#' @keywords internal

#' Extract simple moving average trend
#' @noRd
.extract_ma_trend <- function(ts_data, window, .quiet) {
  # Validate window parameter
  n <- length(ts_data)
  if (window < 2) {
    cli::cli_abort("Moving average window must be at least 2, got {window}")
  }
  if (window > n) {
    cli::cli_abort(
      "Moving average window ({window}) cannot exceed series length ({n})"
    )
  }

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

  # TTR::SMA returns NAs for the first (window-1) observations
  # This is the expected behavior for moving averages

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
  # Validate alpha parameter
  if (alpha <= 0 || alpha >= 1) {
    cli::cli_abort("EWMA alpha must be between 0 and 1 (exclusive), got {alpha}")
  }

  if (!.quiet) {
    cli::cli_inform("Computing EWMA with alpha = {alpha}")
  }

  return(.ewma(ts_data, alpha))
}

#' Exponentially Weighted Moving Average
#' @noRd
.ewma <- function(ts_data, alpha = 0.1) {
  # TTR::EMA expects n (number of periods), not alpha
  # The relationship is: alpha = 2/(n+1), so n = (2-alpha)/alpha
  n <- round((2 - alpha) / alpha)
  n <- max(2, n)  # Ensure minimum of 2 periods

  # Use TTR's optimized EMA implementation (C code)
  ema_result <- TTR::EMA(as.numeric(ts_data), n = n)

  # TTR::EMA already handles NAs appropriately at the beginning
  # No need to replace them with original values

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
  # Validate parameters
  n <- length(ts_data)
  if (window < 2) {
    cli::cli_abort("ALMA window must be at least 2, got {window}")
  }
  if (window > n) {
    cli::cli_abort("ALMA window ({window}) cannot exceed series length ({n})")
  }
  if (offset <= 0 || offset >= 1) {
    cli::cli_abort("ALMA offset must be between 0 and 1 (exclusive), got {offset}")
  }
  if (sigma <= 0) {
    cli::cli_abort("ALMA sigma must be positive, got {sigma}")
  }

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

  # TTR::ALMA handles NAs appropriately
  # Keep NAs at the beginning as expected for moving averages

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
  # Validate period parameter
  n <- length(ts_data)
  if (period < 2) {
    cli::cli_abort("DEMA period must be at least 2, got {period}")
  }
  if (period > n) {
    cli::cli_abort("DEMA period ({period}) cannot exceed series length ({n})")
  }

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

  # TTR::DEMA handles NAs appropriately
  # Keep NAs at the beginning as expected for moving averages

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
  # Validate period parameter
  n <- length(ts_data)
  if (period < 2) {
    cli::cli_abort("HMA period must be at least 2, got {period}")
  }
  if (period > n) {
    cli::cli_abort("HMA period ({period}) cannot exceed series length ({n})")
  }
  # HMA needs at least sqrt(period) + period observations
  min_required <- ceiling(sqrt(period)) + period
  if (n < min_required) {
    cli::cli_warn(
      "HMA with period {period} needs at least {min_required} observations for reliable results, got {n}"
    )
  }

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

  # TTR::HMA handles NAs appropriately
  # Keep NAs at the beginning as expected for moving averages

  trend_ts <- stats::ts(
    hma_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}