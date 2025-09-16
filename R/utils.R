# Define the null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Auto-detect frequency from date column
#' @noRd
.detect_frequency <- function(dates, .quiet = FALSE) {
  if (!inherits(dates, "Date")) {
    cli::cli_abort("Date column must be of class Date")
  }

  # Remove missing dates
  dates <- dates[!is.na(dates)]

  if (length(dates) < 2) {
    cli::cli_abort("Need at least 2 non-missing dates to detect frequency")
  }

  # Sort dates
  dates <- sort(dates)

  # Calculate differences
  diffs <- as.numeric(diff(dates))

  # Get most common difference
  common_diff <- as.numeric(names(sort(table(diffs), decreasing = TRUE))[1])

  # Map to frequency based on common difference
  frequency <- if (common_diff >= 28 && common_diff <= 32) {
    12  # Monthly (approximately 30 days)
  } else if (common_diff >= 85 && common_diff <= 95) {
    4   # Quarterly (approximately 90 days)
  } else if (common_diff >= 7 && common_diff <= 7) {
    52  # Weekly (exactly 7 days)
  } else if (common_diff >= 1 && common_diff <= 3) {
    252  # Daily (approximately 1-3 days, accounting for weekends)
  } else {
    # Try to infer from number of observations per year, but be more restrictive
    date_range <- as.numeric(max(dates) - min(dates))
    n_obs <- length(dates)
    obs_per_year <- n_obs / (date_range / 365.25)

    # Check if differences are too irregular (high variance)
    diff_variance <- stats::var(diffs)
    diff_mean <- mean(diffs)
    cv <- sqrt(diff_variance) / diff_mean  # coefficient of variation

    # If coefficient of variation is too high, consider it irregular
    if (cv > 0.5) {
      cli::cli_abort(
        "Cannot auto-detect frequency. Irregular time intervals detected.
         Common difference: {common_diff} days, observations per year: {round(obs_per_year, 1)}"
      )
    }

    if (obs_per_year >= 10 && obs_per_year <= 14) {
      12  # Monthly
    } else if (obs_per_year >= 3 && obs_per_year <= 5) {
      4   # Quarterly
    } else {
      cli::cli_abort(
        "Cannot auto-detect frequency. Please specify manually.
         Common difference: {common_diff} days, observations per year: {round(obs_per_year, 1)}"
      )
    }
  }

  if (!.quiet) {
    freq_name <- switch(as.character(frequency),
      "4" = "quarterly",
      "12" = "monthly",
      as.character(frequency)
    )
    cli::cli_inform("Auto-detected frequency: {freq_name} ({frequency})")
  }

  return(frequency)
}

#' Internal function to convert data frame to ts
#' @noRd
.df_to_ts_internal <- function(data, date_col, value_col, frequency) {
  dates <- data[[date_col]]
  values <- data[[value_col]]

  # Remove missing values
  complete_cases <- !is.na(dates) & !is.na(values)
  dates <- dates[complete_cases]
  values <- values[complete_cases]

  if (length(values) == 0) {
    cli::cli_abort("No complete cases found (non-missing date and value)")
  }

  # Sort by date
  order_idx <- order(dates)
  dates <- dates[order_idx]
  values <- values[order_idx]

  # Determine start year and period
  start_date <- min(dates)
  start_year <- lubridate::year(start_date)

  if (frequency == 12) {
    start_period <- lubridate::month(start_date)
  } else if (frequency == 4) {
    start_period <- lubridate::quarter(start_date)
  } else {
    start_period <- 1
  }

  # Create time series
  ts_obj <- stats::ts(values, start = c(start_year, start_period), frequency = frequency)

  return(ts_obj)
}

#' Convert trends list to data frame
#' @noRd
.trends_to_df <- function(trends, date_col, suffix) {
  if (is.null(trends)) {
    return(NULL)
  }

  # Handle single trend vs multiple trends
  if (stats::is.ts(trends)) {
    # Single trend - use generic "trend" name for direct ts input
    trends_list <- list(trend = trends)
  } else {
    # Multiple trends (or single trend passed as list)
    trends_list <- trends
  }

  # Convert each trend to data frame
  trend_dfs <- lapply(names(trends_list), function(method) {
    trend_ts <- trends_list[[method]]
    if (is.null(trend_ts) || all(is.na(trend_ts))) {
      return(NULL)
    }

    # Convert to data frame
    df <- data.frame(
      date = zoo::as.Date.ts(trend_ts),
      value = as.numeric(trend_ts)
    )

    # Create column name
    if (method == "trend") {
      # Direct ts input - use simple "trend" name
      col_name <- "trend"
    } else if (is.null(suffix)) {
      col_name <- paste0("trend_", method)
    } else {
      col_name <- paste0("trend_", method, "_", suffix)
    }

    names(df)[2] <- col_name
    names(df)[1] <- date_col

    return(df)
  })

  # Remove NULL entries
  trend_dfs <- trend_dfs[!sapply(trend_dfs, is.null)]

  if (length(trend_dfs) == 0) {
    return(NULL)
  }

  # Merge all trend data frames
  result <- trend_dfs[[1]]
  if (length(trend_dfs) > 1) {
    for (i in 2:length(trend_dfs)) {
      result <- merge(result, trend_dfs[[i]], by = date_col, all = TRUE)
    }
  }

  return(tibble::as_tibble(result))
}

#' Safe merge that handles naming conflicts
#' @noRd
.safe_merge <- function(data, trends_df, date_col) {
  if (is.null(trends_df)) {
    return(data)
  }

  # Check for existing trend columns and create unique names
  existing_names <- names(data)
  new_names <- names(trends_df)[-1]  # Exclude date column

  # Find conflicts and resolve them
  conflicts <- intersect(existing_names, new_names)
  if (length(conflicts) > 0) {
    # Create mapping for new names
    name_mapping <- new_names
    names(name_mapping) <- new_names

    for (conflict in conflicts) {
      # Find a unique name
      counter <- 1
      new_name <- paste0(conflict, "_", counter)
      while (new_name %in% existing_names || new_name %in% new_names) {
        counter <- counter + 1
        new_name <- paste0(conflict, "_", counter)
      }
      name_mapping[conflict] <- new_name

      # Update trends_df column name
      col_idx <- which(names(trends_df) == conflict)
      names(trends_df)[col_idx] <- new_name
    }

    if (length(conflicts) > 0) {
      cli::cli_inform(
        "Renamed conflicting columns: {.val {conflicts}} -> {.val {name_mapping[conflicts]}}"
      )
    }
  }

  # Perform merge
  result <- merge(data, trends_df, by = date_col, all.x = TRUE)

  return(tibble::as_tibble(result))
}

#' Validate economic frequency
#' @noRd
.validate_economic_frequency <- function(frequency) {
  if (!frequency %in% c(4, 12)) {
    cli::cli_abort(
      "Only monthly (12) and quarterly (4) frequencies are supported for economic data.
       Provided frequency: {frequency}"
    )
  }
  return(TRUE)
}

#' Beveridge-Nelson decomposition
#' @noRd
.beveridge_nelson <- function(ts_data, ar_order = NULL) {
  # Convert to numeric vector
  y <- as.numeric(ts_data)
  n <- length(y)

  # First difference the series
  dy <- diff(y)

  # Estimate AR model for first differences if order not specified
  if (is.null(ar_order)) {
    # Use AIC to select optimal order (max 8 for economic data)
    max_order <- min(8, floor(length(dy)/4))
    if (max_order < 1) {
      ar_order <- 1
    } else {
      aic_values <- numeric(max_order)
      for (i in 1:max_order) {
        tryCatch({
          ar_fit <- stats::arima(dy, order = c(i, 0, 0), include.mean = TRUE)
          aic_values[i] <- ar_fit$aic
        }, error = function(e) aic_values[i] <- Inf)
      }
      ar_order <- which.min(aic_values)
    }
  }

  # Fit AR model
  ar_fit <- stats::arima(dy, order = c(ar_order, 0, 0), include.mean = TRUE)

  # Get AR coefficients
  ar_coefs <- ar_fit$coef[1:ar_order]

  # Calculate long-run multiplier
  if (ar_order > 0) {
    long_run_mult <- 1 / (1 - sum(ar_coefs))
  } else {
    long_run_mult <- 1
  }

  # Calculate permanent component (random walk component)
  permanent <- numeric(n)
  permanent[1] <- y[1]

  for (i in 2:n) {
    permanent[i] <- permanent[i-1] + long_run_mult * (y[i] - y[i-1])
  }

  # Transitory component
  transitory <- y - permanent

  # Return permanent component as trend
  trend_ts <- stats::ts(permanent, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Unobserved Components Model (Local Level) - now uses Kalman implementation
#' @noRd
.ucm_local_level <- function(ts_data) {
  # UCM local level is equivalent to Kalman smoother with equal variance components
  total_var <- stats::var(diff(as.numeric(ts_data)), na.rm = TRUE)
  process_noise <- total_var * 0.5
  measurement_noise <- total_var * 0.5

  # Use the same Kalman implementation
  return(.kalman_smooth(ts_data, measurement_noise, process_noise))
}

#' Hamilton filter
#' @noRd
.hamilton_filter <- function(ts_data, h = 8, p = 4) {
  y <- as.numeric(ts_data)
  n <- length(y)

  if (n <= h + p) {
    cli::cli_abort("Time series too short for Hamilton filter. Need at least {h + p + 1} observations, got {n}")
  }

  # Create data frame for regression
  y_reg <- y[(h+1):n]

  # Create lagged variables
  lags_data <- data.frame(y = y_reg)
  for (i in 1:p) {
    lags_data[[paste0("lag", i)]] <- y[(h+1-i):(n-i)]
  }

  # Fit linear model using built-in lm()
  lm_fit <- stats::lm(y ~ ., data = lags_data)
  residuals <- stats::residuals(lm_fit)

  # The trend is the original series minus the residuals
  trend <- numeric(n)
  trend[1:h] <- y[1:h]  # Use original values for first h observations
  trend[(h+1):n] <- y[(h+1):n] - residuals

  trend_ts <- stats::ts(trend, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Simple Exponential Smoothing - Forecast Package Optimized
#' @noRd
.exp_smoothing_simple <- function(ts_data, alpha = NULL) {
  # Use forecast package's optimized simple exponential smoothing
  if (is.null(alpha)) {
    # Use forecast::ses() for automatic parameter optimization
    tryCatch({
      ses_fit <- forecast::ses(ts_data, h = 0)  # h=0 means no forecasting, just smoothing
      smooth_values <- as.numeric(ses_fit$fitted)

      # Handle first value (ses doesn't smooth the first observation)
      smooth_values[1] <- as.numeric(ts_data)[1]

      trend_ts <- stats::ts(smooth_values, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
      return(trend_ts)
    }, error = function(e) {
      # Fallback to HoltWinters if forecast::ses fails
      return(.exp_smoothing_simple_fallback(ts_data, alpha = 0.3))
    })
  } else {
    # Use HoltWinters with fixed alpha for better performance than manual loop
    hw_fit <- stats::HoltWinters(ts_data, alpha = alpha, beta = FALSE, gamma = FALSE)
    smooth_values <- as.numeric(hw_fit$fitted[, "xhat"])

    # Reconstruct full series (HoltWinters doesn't smooth initial observations)
    full_smooth <- numeric(length(ts_data))
    full_smooth[1] <- as.numeric(ts_data)[1]
    if (length(smooth_values) > 0) {
      start_idx <- length(ts_data) - length(smooth_values) + 1
      full_smooth[start_idx:length(ts_data)] <- smooth_values

      # Fill gap if any
      if (start_idx > 2) {
        full_smooth[2:(start_idx-1)] <- as.numeric(ts_data)[2:(start_idx-1)]
      }
    }

    trend_ts <- stats::ts(full_smooth, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
    return(trend_ts)
  }
}

#' Double Exponential Smoothing (Holt) - Forecast Package Optimized
#' @noRd
.exp_smoothing_double <- function(ts_data, alpha = NULL, beta = NULL) {
  # Use forecast package's optimized Holt method
  if (is.null(alpha) || is.null(beta)) {
    # Use forecast::holt() for automatic parameter optimization
    tryCatch({
      holt_fit <- forecast::holt(ts_data, h = 0)  # h=0 means no forecasting, just smoothing
      smooth_values <- as.numeric(holt_fit$fitted)

      # Handle first value
      smooth_values[1] <- as.numeric(ts_data)[1]

      trend_ts <- stats::ts(smooth_values, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
      return(trend_ts)
    }, error = function(e) {
      # Fallback to HoltWinters if forecast::holt fails
      return(.exp_smoothing_double_fallback(ts_data, alpha = 0.3, beta = 0.1))
    })
  } else {
    # Use HoltWinters with fixed parameters for better performance
    hw_fit <- stats::HoltWinters(ts_data, alpha = alpha, beta = beta, gamma = FALSE)
    smooth_values <- as.numeric(hw_fit$fitted[, "xhat"])

    # Reconstruct full series
    full_smooth <- numeric(length(ts_data))
    full_smooth[1] <- as.numeric(ts_data)[1]
    if (length(smooth_values) > 0) {
      start_idx <- length(ts_data) - length(smooth_values) + 1
      full_smooth[start_idx:length(ts_data)] <- smooth_values

      # Fill gap if any
      if (start_idx > 2) {
        full_smooth[2:(start_idx-1)] <- as.numeric(ts_data)[2:(start_idx-1)]
      }
    }

    trend_ts <- stats::ts(full_smooth, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
    return(trend_ts)
  }
}

#' Exponentially Weighted Moving Average (EWMA) - TTR Optimized
#' @noRd
.ewma <- function(ts_data, alpha = 0.1) {
  # Use TTR's optimized EMA implementation (C code)
  # TTR uses ratio = alpha, which is equivalent to our alpha parameter
  ema_result <- TTR::EMA(as.numeric(ts_data), n = round(2/alpha - 1), ratio = alpha)

  # Handle NAs at the beginning by using original values
  if (any(is.na(ema_result))) {
    na_count <- sum(is.na(ema_result))
    ema_result[1:na_count] <- as.numeric(ts_data)[1:na_count]
  }

  trend_ts <- stats::ts(ema_result, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Adaptive Linear Moving Average (ALMA) - TTR Optimized
#' @noRd
.alma <- function(ts_data, window = 9, offset = 0.85, sigma = 6) {
  y <- as.numeric(ts_data)
  n <- length(y)

  if (window >= n) {
    cli::cli_abort("Window size ({window}) must be less than series length ({n})")
  }

  # Use TTR's optimized ALMA implementation (C code)
  alma_result <- TTR::ALMA(y, n = window, offset = offset, sigma = sigma)

  # Handle NAs at the beginning
  if (any(is.na(alma_result))) {
    na_count <- sum(is.na(alma_result))
    # Fill with simple average for initial values
    for (i in 1:na_count) {
      alma_result[i] <- mean(y[1:i])
    }
  }

  trend_ts <- stats::ts(alma_result, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Double Exponential Moving Average (DEMA) - TTR Optimized
#' @noRd
.dema <- function(ts_data, period = 14) {
  # Use TTR's optimized DEMA implementation (C code)
  dema_result <- TTR::DEMA(as.numeric(ts_data), n = period)

  # Handle NAs at the beginning
  if (any(is.na(dema_result))) {
    na_count <- sum(is.na(dema_result))
    dema_result[1:na_count] <- as.numeric(ts_data)[1:na_count]
  }

  trend_ts <- stats::ts(dema_result, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Hull Moving Average (HMA) - TTR Optimized
#' @noRd
.hma <- function(ts_data, period = 14) {
  y <- as.numeric(ts_data)
  n <- length(y)

  if (period >= n) {
    cli::cli_abort("Period ({period}) must be less than series length ({n})")
  }

  # Use TTR's optimized HMA implementation (C code)
  hma_result <- TTR::HMA(y, n = period)

  # Handle NAs at the beginning
  if (any(is.na(hma_result))) {
    na_count <- sum(is.na(hma_result))
    hma_result[1:na_count] <- y[1:na_count]
  }

  trend_ts <- stats::ts(hma_result, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Get method category for parameter mapping
#' @noRd
.get_method_category <- function(method) {
  categories <- list(
    moving_average = c("ma", "alma", "dema", "hma"),
    smoothing = c("hp", "loess", "spline", "exp_simple", "exp_double", "ewma", "sg", "kernel", "kalman", "wavelet"),
    bandpass = c("bk", "cf", "butter"),
    special = c("stl", "poly", "bn", "hamilton", "ucm")
  )

  for (cat_name in names(categories)) {
    if (method %in% categories[[cat_name]]) {
      return(cat_name)
    }
  }
  return("unknown")
}

#' Map unified parameters to method-specific parameters
#' @noRd
.map_unified_params <- function(method, window = NULL, smoothing = NULL,
                                band = NULL, params = list(), frequency = NULL) {

  category <- .get_method_category(method)
  method_params <- list()

  # Handle window parameter for moving average methods
  if (category == "moving_average" && !is.null(window)) {
    method_params <- switch(method,
      "ma" = list(ma_window = window),
      "alma" = list(alma_window = window),
      "dema" = list(dema_period = window),
      "hma" = list(hma_period = window)
    )
  }

  # Handle smoothing parameter
  if (category == "smoothing" && !is.null(smoothing)) {
    method_params <- switch(method,
      "hp" = list(hp_lambda = if (is.numeric(smoothing) && smoothing < 1) {
        # If smoothing is between 0 and 1, convert to lambda
        if (!is.null(frequency)) {
          if (frequency == 4) 1600 * (1 / smoothing) else 14400 * (1 / smoothing)
        } else {
          smoothing
        }
      } else {
        smoothing
      }),
      "loess" = list(loess_span = smoothing),
      "spline" = list(spline_spar = smoothing),
      "exp_simple" = list(exp_alpha = smoothing),
      "exp_double" = list(exp_alpha = smoothing),
      "ewma" = list(ewma_alpha = smoothing),
      "sg" = list(),  # SG uses window parameter, smoothing not directly applicable
      "kernel" = list(kernel_bandwidth = if (is.numeric(smoothing)) smoothing * 10 else NULL),
      "kalman" = list(kalman_smoothing = smoothing),
      "wavelet" = list(wavelet_threshold = smoothing)
    )
  }

  # Handle band parameter for bandpass filters
  if (category == "bandpass" && !is.null(band)) {
    if (length(band) != 2) {
      cli::cli_abort("Band parameter must be a vector of length 2: c(low, high)")
    }
    method_params <- switch(method,
      "bk" = list(bk_low = band[1], bk_high = band[2]),
      "cf" = list(cf_low = band[1], cf_high = band[2]),
      "butter" = list(butter_cutoff = band[1], butter_order = band[2])
    )
  }

  # Handle special cases
  if (method == "stl" && !is.null(window)) {
    method_params <- list(stl_s_window = window)
  }

  # Handle SG filter - can use window parameter
  if (method == "sg" && !is.null(window)) {
    method_params <- list(sg_window = window)
  }

  # Override with any specific parameters from params list
  specific_params <- switch(method,
    "alma" = params[names(params) %in% c("alma_offset", "alma_sigma")],
    "exp_double" = params[names(params) %in% c("exp_beta")],
    "poly" = params[names(params) %in% c("poly_degree")],
    "bn" = params[names(params) %in% c("bn_ar_order")],
    "hamilton" = params[names(params) %in% c("hamilton_h", "hamilton_p")],
    "sg" = params[names(params) %in% c("sg_poly_order")],
    "kernel" = params[names(params) %in% c("kernel_type")],
    "butter" = params[names(params) %in% c("butter_type")],
    "kalman" = params[names(params) %in% c("kalman_measurement_noise", "kalman_process_noise")],
    "wavelet" = params[names(params) %in% c("wavelet_type")],
    list()
  )

  # Merge parameters, with specific params overriding unified ones
  method_params <- c(method_params, specific_params)

  return(method_params)
}

#' Process all methods with unified parameters
#' @noRd
.process_unified_params <- function(methods, window = NULL, smoothing = NULL,
                                   band = NULL, params = list(), frequency = NULL) {

  all_params <- list()

  for (method in methods) {
    method_params <- .map_unified_params(method, window, smoothing, band, params, frequency)
    all_params <- c(all_params, method_params)
  }

  # Remove duplicates (keep first occurrence)
  all_params <- all_params[!duplicated(names(all_params))]

  return(all_params)
}

#' Check for deprecated parameters and issue warnings
#' @noRd
.check_deprecated_params <- function(...) {
  dots <- list(...)
  deprecated_params <- c(
    "hp_lambda", "ma_window", "stl_s_window", "loess_span", "spline_spar",
    "ewma_alpha", "alma_window", "dema_period", "hma_period",
    "bk_low", "bk_high", "cf_low", "cf_high"
  )

  used_deprecated <- intersect(names(dots), deprecated_params)

  if (length(used_deprecated) > 0) {
    cli::cli_warn(c(
      "!" = "Using deprecated parameter{?s}: {.arg {used_deprecated}}",
      "i" = "Consider using the unified parameters: {.arg window}, {.arg smoothing}, or {.arg band}",
      "i" = "See {.code ?extract_trends} for the new simplified API"
    ))
  }

  return(invisible(NULL))
}

#' Savitzky-Golay filter (optimized with pre-computed coefficients)
#' @noRd
.savitzky_golay <- function(ts_data, window = 7, poly_order = 2) {
  y <- as.numeric(ts_data)
  n <- length(y)

  if (window >= n) {
    cli::cli_abort("Window size ({window}) must be less than series length ({n})")
  }

  if (window %% 2 == 0) {
    window <- window + 1  # Ensure odd window size
  }

  # Use signal package if available for optimized implementation
  if (requireNamespace("signal", quietly = TRUE)) {
    return(.savitzky_golay_signal(ts_data, window, poly_order))
  } else {
    return(.savitzky_golay_custom(ts_data, window, poly_order))
  }
}

#' Savitzky-Golay using signal package
#' @noRd
.savitzky_golay_signal <- function(ts_data, window, poly_order) {
  y <- as.numeric(ts_data)

  # Use signal package's sgolayfilt function
  smoothed <- signal::sgolayfilt(y, p = poly_order, n = window)

  trend_ts <- stats::ts(smoothed, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Custom Savitzky-Golay with pre-computed coefficients
#' @noRd
.savitzky_golay_custom <- function(ts_data, window, poly_order) {
  y <- as.numeric(ts_data)
  n <- length(y)

  half_window <- (window - 1) / 2

  # Pre-compute Savitzky-Golay coefficients for the center case
  x_vec <- seq(-half_window, half_window)
  X <- outer(x_vec, 0:poly_order, "^")

  # Compute coefficients using least squares (only need center point)
  coeffs <- solve(t(X) %*% X) %*% t(X)
  center_coeffs <- coeffs[1, ]  # Coefficients for zeroth derivative (smoothing)

  smoothed <- numeric(n)

  # Apply filter
  for (i in 1:n) {
    left <- max(1, i - half_window)
    right <- min(n, i + half_window)

    if (right - left + 1 == window) {
      # Full window case - use pre-computed coefficients
      smoothed[i] <- sum(center_coeffs * y[left:right])
    } else {
      # Edge case - use adaptive local polynomial
      x_local <- seq(left, right) - i
      y_local <- y[left:right]

      if (length(y_local) > poly_order) {
        poly_fit <- stats::lm(y_local ~ stats::poly(x_local, degree = poly_order, raw = TRUE))
        smoothed[i] <- stats::predict(poly_fit, newdata = data.frame(x_local = 0))
      } else {
        smoothed[i] <- y[i]
      }
    }
  }

  trend_ts <- stats::ts(smoothed, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Kernel regression smoother
#' @noRd
.kernel_smooth <- function(ts_data, bandwidth = NULL, kernel = "normal") {
  y <- as.numeric(ts_data)
  n <- length(y)
  x <- 1:n

  # Auto-select bandwidth if not provided
  if (is.null(bandwidth)) {
    bandwidth <- n / 10  # Default: 10% of series length
  }

  # Use built-in ksmooth function
  smoothed_result <- stats::ksmooth(x, y, kernel = kernel, bandwidth = bandwidth, x.points = x)

  trend_ts <- stats::ts(smoothed_result$y, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Butterworth filter
#' @noRd
.butterworth_filter <- function(ts_data, cutoff = 0.1, order = 2) {
  # Check if signal package is available
  if (!requireNamespace("signal", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg signal} not available. Using simple low-pass filter instead.")
    return(.simple_lowpass(ts_data, cutoff))
  }

  y <- as.numeric(ts_data)
  n <- length(y)

  # Create Butterworth filter
  bf <- signal::butter(order, cutoff, type = "low")

  # Apply filter (forward and backward to avoid phase shift)
  filtered <- signal::filtfilt(bf, y)

  trend_ts <- stats::ts(filtered, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Simple low-pass filter (fallback for Butterworth)
#' @noRd
.simple_lowpass <- function(ts_data, cutoff = 0.1) {
  y <- as.numeric(ts_data)
  n <- length(y)

  # Simple moving average as low-pass filter
  window_size <- max(3, round(1 / cutoff))
  if (window_size >= n) window_size <- n - 1

  # Apply centered moving average
  filtered <- stats::filter(y, filter = rep(1/window_size, window_size), method = "convolution")

  # Fill NAs at edges with original values
  filtered[is.na(filtered)] <- y[is.na(filtered)]

  trend_ts <- stats::ts(filtered, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Kalman smoother (using dlm package or fallback)
#' @noRd
.kalman_smooth <- function(ts_data, measurement_noise = NULL, process_noise = NULL) {
  # Try using dlm package for robust implementation
  if (requireNamespace("dlm", quietly = TRUE)) {
    return(.kalman_dlm(ts_data, measurement_noise, process_noise))
  } else {
    # Fallback to custom implementation
    return(.kalman_custom(ts_data, measurement_noise, process_noise))
  }
}

#' Kalman smoother using dlm package
#' @noRd
.kalman_dlm <- function(ts_data, measurement_noise = NULL, process_noise = NULL) {
  y <- as.numeric(ts_data)
  n <- length(y)

  # Estimate noise parameters if not provided
  if (is.null(measurement_noise)) {
    measurement_noise <- stats::var(diff(y), na.rm = TRUE) * 0.1
  }
  if (is.null(process_noise)) {
    process_noise <- stats::var(diff(y), na.rm = TRUE) * 0.01
  }

  # Build local level model
  mod <- dlm::dlmModPoly(order = 1, dV = measurement_noise, dW = process_noise)

  # Apply Kalman smoother
  filtered <- dlm::dlmFilter(y, mod)
  smoothed <- dlm::dlmSmooth(filtered)

  # Extract smoothed states (handle different dlm output structures)
  if (is.matrix(smoothed$s)) {
    trend_values <- smoothed$s[-1, 1]  # Remove initial state, take first column
  } else {
    trend_values <- as.numeric(smoothed$s[-1])  # Remove initial state
  }

  trend_ts <- stats::ts(trend_values, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Custom Kalman smoother implementation (fallback)
#' @noRd
.kalman_custom <- function(ts_data, measurement_noise = NULL, process_noise = NULL) {
  y <- as.numeric(ts_data)
  n <- length(y)

  # Estimate noise parameters if not provided
  if (is.null(measurement_noise)) {
    measurement_noise <- stats::var(diff(y), na.rm = TRUE) * 0.1
  }
  if (is.null(process_noise)) {
    process_noise <- stats::var(diff(y), na.rm = TRUE) * 0.01
  }

  # Initialize Kalman filter
  x <- numeric(n)  # State estimates
  P <- numeric(n)  # Error covariances

  # Initial conditions
  x[1] <- y[1]
  P[1] <- measurement_noise

  # Forward pass (filtering)
  for (t in 2:n) {
    # Prediction step
    x_pred <- x[t-1]
    P_pred <- P[t-1] + process_noise

    # Update step
    if (!is.na(y[t])) {
      K <- P_pred / (P_pred + measurement_noise)  # Kalman gain
      x[t] <- x_pred + K * (y[t] - x_pred)
      P[t] <- (1 - K) * P_pred
    } else {
      x[t] <- x_pred
      P[t] <- P_pred
    }
  }

  # Backward pass (smoothing)
  x_smooth <- x
  for (t in (n-1):1) {
    if (P[t] > 0) {
      A <- P[t] / (P[t] + process_noise)
      x_smooth[t] <- x[t] + A * (x_smooth[t+1] - x[t])
    }
  }

  trend_ts <- stats::ts(x_smooth, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Wavelet denoising
#' @noRd
.wavelet_denoise <- function(ts_data, threshold = NULL, wavelet = "haar") {
  # Check if wavelets package is available
  if (!requireNamespace("wavelets", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg wavelets} not available. Using simple smoothing instead.")
    return(.simple_smooth(ts_data, threshold))
  }

  y <- as.numeric(ts_data)
  n <- length(y)

  # Pad to power of 2 if necessary
  if (n != 2^floor(log2(n))) {
    pad_length <- 2^ceiling(log2(n))
    y_padded <- c(y, rep(y[n], pad_length - n))
  } else {
    y_padded <- y
    pad_length <- n
  }

  # Wavelet decomposition
  wt <- wavelets::dwt(y_padded, filter = wavelet)

  # Automatic threshold if not provided
  if (is.null(threshold)) {
    threshold <- stats::sd(y, na.rm = TRUE) * 0.1
  }

  # Soft thresholding
  for (i in 1:length(wt@W)) {
    wt@W[[i]] <- sign(wt@W[[i]]) * pmax(0, abs(wt@W[[i]]) - threshold)
  }

  # Reconstruct
  denoised <- wavelets::idwt(wt)

  # Remove padding and return original length
  denoised <- denoised[1:n]

  trend_ts <- stats::ts(denoised, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Simple smoothing (fallback for wavelet)
#' @noRd
.simple_smooth <- function(ts_data, smoothing_param = 0.1) {
  y <- as.numeric(ts_data)
  n <- length(y)

  # Simple exponential smoothing as fallback
  alpha <- if (is.null(smoothing_param) || length(smoothing_param) == 0) 0.1 else smoothing_param
  smooth <- numeric(n)
  smooth[1] <- y[1]

  for (i in 2:n) {
    smooth[i] <- alpha * y[i] + (1 - alpha) * smooth[i-1]
  }

  trend_ts <- stats::ts(smooth, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Simple Exponential Smoothing Fallback
#' @noRd
.exp_smoothing_simple_fallback <- function(ts_data, alpha = 0.3) {
  y <- as.numeric(ts_data)
  n <- length(y)

  # Simple exponential smoothing using vectorized approach
  smooth <- stats::filter(y, filter = alpha, method = "recursive", init = y[1])
  trend_ts <- stats::ts(as.numeric(smooth), start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}

#' Double Exponential Smoothing Fallback
#' @noRd
.exp_smoothing_double_fallback <- function(ts_data, alpha = 0.3, beta = 0.1) {
  # Use HoltWinters as fallback
  hw_fit <- stats::HoltWinters(ts_data, alpha = alpha, beta = beta, gamma = FALSE)
  smooth_values <- as.numeric(hw_fit$fitted[, "xhat"])

  # Reconstruct full series
  full_smooth <- numeric(length(ts_data))
  full_smooth[1] <- as.numeric(ts_data)[1]
  if (length(smooth_values) > 0) {
    start_idx <- length(ts_data) - length(smooth_values) + 1
    full_smooth[start_idx:length(ts_data)] <- smooth_values
    if (start_idx > 2) {
      full_smooth[2:(start_idx-1)] <- as.numeric(ts_data)[2:(start_idx-1)]
    }
  }

  trend_ts <- stats::ts(full_smooth, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
  return(trend_ts)
}