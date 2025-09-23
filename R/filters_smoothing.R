#' Statistical Smoothing Methods
#'
#' @description Internal functions for statistical smoothing and regression-based
#' trend extraction methods including loess, splines, polynomial fitting, STL
#' decomposition, and exponential smoothing variants.
#'
#' @name smoothing-filters
#' @keywords internal

#' Extract loess trend
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

#' Extract spline trend
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

#' Extract polynomial trend
#' @noRd
.extract_poly_trend <- function(ts_data, degree, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing polynomial trend with degree = {degree}")
  }

  # Create time index
  time_index <- as.numeric(stats::time(ts_data))
  values <- as.numeric(ts_data)

  # Fit polynomial
  poly_fit <- stats::lm(
    values ~ stats::poly(time_index, degree = degree, raw = TRUE)
  )
  trend_values <- stats::fitted(poly_fit)

  # Convert back to ts
  trend <- stats::ts(
    trend_values,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend)
}

#' Extract STL trend
#' @noRd
.extract_stl_trend <- function(ts_data, s_window, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing STL trend with s.window = {s_window}")
  }

  # Check if series has enough seasonality for STL
  freq <- stats::frequency(ts_data)
  if (freq == 1) {
    cli::cli_warn(
      "STL not applicable for non-seasonal data. Using HP filter instead."
    )
    return(.extract_hp_trend(ts_data, lambda = 1600, .quiet = TRUE))
  }

  stl_result <- stats::stl(ts_data, s.window = s_window)
  return(stl_result$time.series[, "trend"])
}

#' Extract simple exponential smoothing trend
#' @noRd
.extract_exp_simple_trend <- function(ts_data, alpha, .quiet) {
  if (!.quiet) {
    msg <- if (is.null(alpha)) "optimized alpha" else "alpha = {alpha}"
    cli::cli_inform("Computing simple exponential smoothing with {msg}")
  }

  return(.exp_smoothing_simple(ts_data, alpha))
}

#' Simple exponential smoothing using forecast package
#' @noRd
.exp_smoothing_simple <- function(ts_data, alpha = NULL) {
  # Use forecast package's optimized simple exponential smoothing
  if (is.null(alpha)) {
    # Use forecast::ses() for automatic parameter optimization
    tryCatch(
      {
        ses_fit <- forecast::ses(ts_data, h = 0) # h=0 means no forecasting, just smoothing
        smooth_values <- as.numeric(ses_fit$fitted)

        # Handle first value (ses doesn't smooth the first observation)
        smooth_values[1] <- as.numeric(ts_data)[1]

        trend_ts <- stats::ts(
          smooth_values,
          start = stats::start(ts_data),
          frequency = stats::frequency(ts_data)
        )
        return(trend_ts)
      },
      error = function(e) {
        # Fallback to HoltWinters if forecast::ses fails
        return(.exp_smoothing_simple_fallback(ts_data, alpha = 0.3))
      }
    )
  } else {
    # Use HoltWinters with fixed alpha for better performance than manual loop
    hw_fit <- stats::HoltWinters(
      ts_data,
      alpha = alpha,
      beta = FALSE,
      gamma = FALSE
    )
    smooth_values <- as.numeric(hw_fit$fitted[, "xhat"])

    # Reconstruct full series (HoltWinters doesn't smooth initial observations)
    full_smooth <- numeric(length(ts_data))
    full_smooth[1] <- as.numeric(ts_data)[1]
    if (length(smooth_values) > 0) {
      start_idx <- length(ts_data) - length(smooth_values) + 1
      full_smooth[start_idx:length(ts_data)] <- smooth_values

      # Fill gap if any
      if (start_idx > 2) {
        full_smooth[2:(start_idx - 1)] <- as.numeric(ts_data)[2:(start_idx - 1)]
      }
    }

    trend_ts <- stats::ts(
      full_smooth,
      start = stats::start(ts_data),
      frequency = stats::frequency(ts_data)
    )
    return(trend_ts)
  }
}

#' Extract double exponential smoothing trend
#' @noRd
.extract_exp_double_trend <- function(ts_data, alpha, beta, .quiet) {
  if (!.quiet) {
    alpha_msg <- if (is.null(alpha)) "0.3" else "{alpha}"
    beta_msg <- if (is.null(beta)) "0.1" else "{beta}"
    cli::cli_inform(
      "Computing double exponential smoothing with alpha = {alpha_msg}, beta = {beta_msg}"
    )
  }

  return(.exp_smoothing_double(ts_data, alpha, beta))
}

#' Double exponential smoothing using forecast package
#' @noRd
.exp_smoothing_double <- function(ts_data, alpha = NULL, beta = NULL) {
  # Use forecast package's optimized Holt method
  if (is.null(alpha) || is.null(beta)) {
    # Use forecast::holt() for automatic parameter optimization
    tryCatch(
      {
        holt_fit <- forecast::holt(ts_data, h = 0) # h=0 means no forecasting, just smoothing
        smooth_values <- as.numeric(holt_fit$fitted)

        # Handle first value
        smooth_values[1] <- as.numeric(ts_data)[1]

        trend_ts <- stats::ts(
          smooth_values,
          start = stats::start(ts_data),
          frequency = stats::frequency(ts_data)
        )
        return(trend_ts)
      },
      error = function(e) {
        # Fallback to HoltWinters if forecast::holt fails
        return(.exp_smoothing_double_fallback(ts_data, alpha = 0.3, beta = 0.1))
      }
    )
  } else {
    # Use HoltWinters with fixed parameters for better performance
    hw_fit <- stats::HoltWinters(
      ts_data,
      alpha = alpha,
      beta = beta,
      gamma = FALSE
    )
    smooth_values <- as.numeric(hw_fit$fitted[, "xhat"])

    # Reconstruct full series
    full_smooth <- numeric(length(ts_data))
    full_smooth[1] <- as.numeric(ts_data)[1]
    if (length(smooth_values) > 0) {
      start_idx <- length(ts_data) - length(smooth_values) + 1
      full_smooth[start_idx:length(ts_data)] <- smooth_values

      # Fill gap if any
      if (start_idx > 2) {
        full_smooth[2:(start_idx - 1)] <- as.numeric(ts_data)[2:(start_idx - 1)]
      }
    }

    trend_ts <- stats::ts(
      full_smooth,
      start = stats::start(ts_data),
      frequency = stats::frequency(ts_data)
    )
    return(trend_ts)
  }
}

#' Simple Exponential Smoothing Fallback
#' @noRd
.exp_smoothing_simple_fallback <- function(ts_data, alpha = 0.3) {
  # Vectorized manual implementation for performance
  # Similar to stats::filter(y, filter = alpha, method = "recursive", init = y[1])

  # This is a faster approach than manual loops:
  # y <- as.numeric(ts_data)
  # smooth <- stats::filter(y, filter = alpha, method = "recursive", init = y[1])
  # trend_ts <- stats::ts(as.numeric(smooth), start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  # return(trend_ts)

  hw <- stats::HoltWinters(
    ts_data,
    alpha = alpha,
    beta = FALSE,
    gamma = FALSE
  )

  smooth_values <- as.numeric(hw$fitted[, "xhat"])

  # HoltWinters loses 1 observation for simple smoothing
  full_smooth <- numeric(length(ts_data))
  full_smooth[1] <- as.numeric(ts_data)[1] # First value used as initial level
  full_smooth[2:length(ts_data)] <- smooth_values

  trend_ts <- stats::ts(
    full_smooth,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Double Exponential Smoothing Fallback
#' @noRd
.exp_smoothing_double_fallback <- function(ts_data, alpha = 0.3, beta = 0.1) {
  # Use HoltWinters as fallback
  hw_fit <- stats::HoltWinters(
    ts_data,
    alpha = alpha,
    beta = beta,
    gamma = FALSE
  )
  smooth_values <- as.numeric(hw_fit$fitted[, "xhat"])

  # Reconstruct full series
  full_smooth <- as.numeric(length(ts_data))
  full_smooth[1] <- as.numeric(ts_data)[1]
  if (length(smooth_values) > 0) {
    start_idx <- length(ts_data) - length(smooth_values) + 1
    full_smooth[start_idx:length(ts_data)] <- smooth_values

    # Fill gap if any
    if (start_idx > 2) {
      full_smooth[2:(start_idx - 1)] <- as.numeric(ts_data)[2:(start_idx - 1)]
    }
  }

  trend_ts <- stats::ts(
    full_smooth,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}