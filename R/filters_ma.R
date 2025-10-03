#' Moving Average Filtering Methods
#'
#' @description Internal functions for various moving average trend extraction methods.
#' These methods use C-optimized implementations (RcppRoll) for performance where possible,
#' with custom R implementations for exponential moving averages.
#'
#' @details
#' All moving average functions use C-optimized implementations (via RcppRoll) for speed.
#' NAs are preserved at the beginning of the series as expected for moving averages.
#'
#' Parameter notes:
#' - **SMA**: window parameter specifies the number of periods
#' - **WMA**: weighted MA with custom or linear weights
#' - **EWMA**: alpha parameter (0 < alpha < 1) controls smoothing strength
#' - **ZLEMA**: zero-lag EMA reduces lag while maintaining smoothness
#' - **Triangular**: double-smoothed MA with triangular weights
#'
#' @name ma-filters
#' @keywords internal

#' Extract simple moving average trend
#' @noRd
.extract_ma_trend <- function(ts_data, window, align, .quiet) {
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

  # Validate align parameter
  if (!align %in% c("left", "center", "right")) {
    cli::cli_abort(
      "Moving average align must be 'left', 'center', or 'right', got {.val {align}}"
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
    cli::cli_inform(
      "Computing {msg}-period moving average with {align} alignment"
    )
  }

  return(.sma(ts_data, window, align))
}

#' Simple Moving Average with alignment options
#' @noRd
.sma <- function(ts_data, window = 10, align = "center") {
  # Use RcppRoll for C++ optimized rolling mean
  ma_result <- RcppRoll::roll_mean(
    as.numeric(ts_data),
    n = window,
    align = align,
    fill = NA,
    na.rm = FALSE
  )

  # Convert back to ts object
  trend_ts <- stats::ts(
    ma_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract EWMA trend
#' @noRd
.extract_ewma_trend <- function(ts_data, window = NULL, alpha = NULL, .quiet) {
  # Validate parameters - exactly one should be provided
  if (!is.null(window) && !is.null(alpha)) {
    cli::cli_abort("Provide either 'window' or 'alpha' for EWMA, not both")
  }

  # Default to alpha if neither provided
  if (is.null(window) && is.null(alpha)) {
    alpha <- 0.1
  }

  # Validate window if provided
  if (!is.null(window)) {
    n <- length(ts_data)
    if (window < 2) {
      cli::cli_abort("EWMA window must be at least 2, got {window}")
    }
    if (window > n) {
      cli::cli_abort("EWMA window ({window}) cannot exceed series length ({n})")
    }
  }

  # Validate alpha if provided
  if (!is.null(alpha)) {
    if (alpha <= 0 || alpha >= 1) {
      cli::cli_abort(
        "EWMA alpha must be between 0 and 1 (exclusive), got {alpha}"
      )
    }
  }

  if (!.quiet) {
    if (!is.null(window)) {
      cli::cli_inform("Computing EWMA with window = {window}")
    } else {
      cli::cli_inform("Computing EWMA with alpha = {alpha}")
    }
  }

  return(.ewma(ts_data, window = window, alpha = alpha))
}

#' Exponentially Weighted Moving Average
#' @noRd
.ewma <- function(ts_data, window = NULL, alpha = NULL) {
  # Default to alpha if neither provided
  if (is.null(window) && is.null(alpha)) {
    alpha <- 0.1
  }

  y <- as.numeric(ts_data)

  if (!is.null(window)) {
    # Calculate alpha from window parameter (matching TTR formula)
    alpha <- 2 / (window + 1)

    # Use custom EMA implementation
    n <- length(y)
    ema_result <- numeric(n)
    ema_result[1] <- y[1]  # Initialize with first value

    for (i in 2:n) {
      ema_result[i] <- alpha * y[i] + (1 - alpha) * ema_result[i - 1]
    }
  } else {
    # Traditional EWMA implementation with alpha parameter
    n <- length(y)
    ema_result <- numeric(n)
    ema_result[1] <- y[1] # Initialize with first value

    # Apply exponential smoothing formula: S_t = alpha * y_t + (1 - alpha) * S_{t-1}
    for (i in 2:n) {
      ema_result[i] <- alpha * y[i] + (1 - alpha) * ema_result[i - 1]
    }
  }

  # Convert back to ts object
  trend_ts <- stats::ts(
    ema_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}


#' Extract WMA trend
#' @noRd
.extract_wma_trend <- function(ts_data, window, weights, align, .quiet) {
  # Validate window parameter
  n <- length(ts_data)
  if (window < 2) {
    cli::cli_abort("WMA window must be at least 2, got {window}")
  }
  if (window > n) {
    cli::cli_abort(
      "WMA window ({window}) cannot exceed series length ({n})"
    )
  }

  # Validate weights if provided
  if (!is.null(weights)) {
    if (length(weights) != window) {
      cli::cli_abort(
        "WMA weights length ({length(weights)}) must match window ({window})"
      )
    }
    if (!is.numeric(weights) || any(weights < 0)) {
      cli::cli_abort("WMA weights must be non-negative numeric values")
    }
  }

  # Validate align parameter
  if (!align %in% c("left", "center", "right")) {
    cli::cli_abort(
      "WMA align must be 'left', 'center', or 'right', got {.val {align}}"
    )
  }

  if (!.quiet) {
    weight_msg <- if (is.null(weights)) "linear weights" else "custom weights"
    cli::cli_inform(
      "Computing {window}-period weighted MA with {weight_msg}, {align} alignment"
    )
  }

  return(.wma(ts_data, window, weights, align))
}

#' Weighted Moving Average (WMA)
#' @noRd
.wma <- function(ts_data, window = 10, weights = NULL, align = "center") {
  # Default to linear weights if not provided
  if (is.null(weights)) {
    weights <- 1:window
  }

  # Normalize weights
  weights <- weights / sum(weights)

  # Use RcppRoll for C++ optimized rolling mean with weights
  wma_result <- RcppRoll::roll_mean(
    as.numeric(ts_data),
    n = window,
    weights = weights,
    align = align,
    fill = NA,
    na.rm = FALSE
  )

  # Convert back to ts object
  trend_ts <- stats::ts(
    wma_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract ZLEMA trend
#' @noRd
.extract_zlema_trend <- function(ts_data, window, ratio, .quiet) {
  # Validate window parameter
  n <- length(ts_data)
  if (window < 2) {
    cli::cli_abort("ZLEMA window must be at least 2, got {window}")
  }
  if (window > n) {
    cli::cli_abort(
      "ZLEMA window ({window}) cannot exceed series length ({n})"
    )
  }

  # Validate ratio if provided
  if (!is.null(ratio)) {
    if (!is.numeric(ratio) || length(ratio) != 1 || ratio < 0 || ratio > 1) {
      cli::cli_abort(
        "ZLEMA ratio must be a single numeric value between 0 and 1, got {ratio}"
      )
    }
  }

  if (!.quiet) {
    ratio_msg <- if (is.null(ratio)) "auto ratio" else "ratio = {ratio}"
    cli::cli_inform("Computing ZLEMA with window = {window}, {ratio_msg}")
  }

  return(.zlema(ts_data, window, ratio))
}

#' Zero Lag Exponential Moving Average (ZLEMA)
#' @noRd
.zlema <- function(ts_data, window = 10, ratio = NULL) {
  y <- as.numeric(ts_data)
  n <- length(y)

  # Calculate lag (how many periods to look back)
  if (is.null(ratio)) {
    lag <- floor((window - 1) / 2)
  } else {
    lag <- floor(ratio * (window - 1))
  }

  # Initialize result vector
  zlema_result <- rep(NA_real_, n)

  # Calculate EMA weight (alpha)
  alpha <- 2 / (window + 1)

  # Start ZLEMA calculation after we have enough data
  start_idx <- lag + 1
  if (start_idx <= n) {
    # Initialize with first available value
    zlema_result[start_idx] <- y[start_idx]

    # Calculate ZLEMA using lag-adjusted values
    for (i in (start_idx + 1):n) {
      # Lag-adjusted value: current value + (current - lagged)
      lag_adjusted <- y[i] + (y[i] - y[i - lag])
      # Apply EMA formula to lag-adjusted value
      zlema_result[i] <- alpha * lag_adjusted + (1 - alpha) * zlema_result[i - 1]
    }
  }

  # Convert back to ts object
  trend_ts <- stats::ts(
    zlema_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract Triangular MA trend
#' @noRd
.extract_triangular_trend <- function(ts_data, window, align, .quiet) {
  # Validate window parameter
  n <- length(ts_data)
  if (window < 3) {
    cli::cli_abort("Triangular MA window must be at least 3, got {window}")
  }
  if (window > n) {
    cli::cli_abort(
      "Triangular MA window ({window}) cannot exceed series length ({n})"
    )
  }

  # Validate align parameter
  if (!align %in% c("center", "right")) {
    cli::cli_abort(
      "Triangular MA align must be 'center' or 'right', got {.val {align}}"
    )
  }

  if (!.quiet) {
    cli::cli_inform(
      "Computing {window}-period triangular MA with {align} alignment"
    )
  }

  return(.triangular(ts_data, window, align))
}

#' Triangular Moving Average (custom implementation using stats::filter)
#' @noRd
.triangular <- function(ts_data, window = 10, align = "center") {
  # Create triangular weights
  if (window %% 2 == 1) {
    # Odd window: symmetric triangle with peak at center
    mid <- (window + 1) / 2
    weights <- c(1:mid, (mid - 1):1)
  } else {
    # Even window: two middle values are equal (flat peak)
    mid <- window / 2
    weights <- c(1:mid, mid:1)
  }

  # For right alignment, reverse weights so recent observations get higher weights
  if (align == "right") {
    weights <- rev(weights)
  }

  # Normalize weights to sum to 1
  weights <- weights / sum(weights)

  # Set sides parameter based on alignment
  sides <- if (align == "center") 2L else 1L

  # Use stats::filter for efficient convolution
  result <- stats::filter(
    as.numeric(ts_data),
    filter = weights,
    method = "convolution",
    sides = sides
  )

  # Convert back to ts object
  trend_ts <- stats::ts(
    as.numeric(result),
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend_ts)
}
