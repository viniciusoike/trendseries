#' Econometric Filtering Methods
#'
#' @description Internal functions for econometric trend extraction methods.
#' These methods are commonly used in macroeconomic analysis for business cycle
#' decomposition and trend extraction.
#'
#' @name econometric-filters
#' @keywords internal

#' Extract HP trend
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
    hp_result[, 1], # hp2 returns data.frame with single column
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend)
}

#' Extract Baxter-King trend
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

#' Extract Christiano-Fitzgerald trend
#' @noRd
.extract_cf_trend <- function(ts_data, pl, pu, .quiet) {
  if (!.quiet) {
    cli::cli_inform(
      "Computing Christiano-Fitzgerald filter with bands [{pl}, {pu}]"
    )
  }

  if (!requireNamespace("mFilter", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg mFilter} is required for Christiano-Fitzgerald filter"
    )
  }

  cf_result <- mFilter::cffilter(ts_data, pl = pl, pu = pu)
  return(cf_result$trend)
}

#' Extract Hamilton trend
#' @noRd
.extract_hamilton_trend <- function(ts_data, h, p, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing Hamilton filter with h = {h}, p = {p}")
  }

  return(.hamilton_filter(ts_data, h, p))
}

#' Get Hamilton filter parameters based on frequency
#' @noRd
.get_hamilton_params <- function(frequency, smooth_level = "medium") {
  params <- list(
    # Monthly
    "12" = list(
      light = list(h = 12, p = 12),
      medium = list(h = 24, p = 12),
      heavy = list(h = 36, p = 12)
    ),
    # Quarterly
    "4" = list(
      light = list(h = 4, p = 4),
      medium = list(h = 8, p = 4),
      heavy = list(h = 12, p = 4)
    ),
    # Annual
    "1" = list(
      light = list(h = 1, p = 1),
      medium = list(h = 2, p = 1),
      heavy = list(h = 3, p = 1)
    )
  )

  freq_key <- as.character(frequency)
  if (freq_key %in% names(params)) {
    return(params[[freq_key]][[smooth_level]])
  } else {
    # Default fallback
    return(list(h = 2 * frequency, p = frequency))
  }
}

#' Hamilton filter
#'
#' Implements the Hamilton filter for trend extraction.
#'
#' @details
#' This is a simple implementation that always assumes the process is not a
#' random walk with drift. It uses OLS regression to estimate the trend
#' component based on future values and uses a simple linear interpolation
#' to fill in missing values at the start and end of the series.
#'
#' In the future, we may implement more options for finer control including:
#'
#' - A choice for the specification of the regression ('auto', 'rw', 'regression')
#' - Different methods to handle the start and end of the series
#' - Nicer defaults for p and h based on frequency and smoothing level
#'
#' @noRd
.hamilton_filter <- function(ts_data, h = NULL, p = NULL) {
  y <- as.numeric(ts_data)
  n <- length(y)

  if (is.null(h) & is.null(p)) {
    cli::cli_inform(
      "Using default Hamilton filter parameters based on frequency and medium smoothing."
    )

    freq <- stats::frequency(ts_data)
    params <- .get_hamilton_params(freq, smooth_level = "medium")
    h <- params$h
    p <- params$p
  }

  if (n <= h + p) {
    cli::cli_abort(
      "Time series too short for Hamilton filter. Need at least {h + p + 1} observations, got {n}"
    )
  }

  # Hamilton regression: y_{t+h} = β₀ + β₁*y_t + ... + β_p*y_{t-p+1} + ε_{t+h}
  # The fitted values from this regression give us the trend component at time t+h

  # Create lagged matrix more efficiently
  # We're predicting y_{t+h} from y_t, y_{t-1}, ..., y_{t-p+1}
  X <- matrix(NA, nrow = n - h - p + 1, ncol = p + 1)
  X[, 1] <- 1 # Intercept

  for (j in 1:p) {
    X[, j + 1] <- y[(p - j + 1):(n - h - j + 1)]
  }

  # Dependent variable: y_{t+h}
  y_future <- y[(p + h):n]

  # Solve using QR decomposition (more stable than lm() for this case)
  qr_decomp <- qr(X)
  coef <- qr.coef(qr_decomp, y_future)

  # Calculate fitted values (this is our trend estimate shifted by h periods)
  fitted_vals <- X %*% coef

  # The residuals are the cyclical component
  cycle_future <- y_future - fitted_vals

  # Construct the full trend series
  trend <- as.numeric(n)

  # For the first p-1 observations, we can't run the regression
  # Hamilton suggests using the original values or NA
  trend[1:(p - 1)] <- NA

  # For observations p through n-h, we have the fitted values
  # Note: fitted_vals[i] corresponds to the trend at position p + i - 1
  for (i in 1:length(fitted_vals)) {
    pos <- p + i - 1
    if (pos <= n) {
      trend[pos] <- fitted_vals[i]
    }
  }

  # For the last h observations, we need a different approach
  # Option 1: Extend using the last available regression
  # Option 2: Use NA (Hamilton's original approach)
  # Option 3: Use a forecasting method

  # Currently only uses linear interpolation.
  # Implement options in the future

  # Fill in missing values with original series or interpolation
  # This is often done in practice
  na_idx <- which(is.na(trend))
  if (length(na_idx) > 0) {
    # Use linear interpolation for internal NAs
    trend <- zoo::na.fill(trend, "extend")
  }

  trend_ts <- stats::ts(
    trend,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract Beveridge-Nelson trend
#' @noRd
.extract_bn_trend <- function(ts_data, .quiet) {
  # if (!.quiet) {
  #   msg <- if (is.null(ar_order)) {
  #     "automatic AR order selection"
  #   } else {
  #     "AR({ar_order})"
  #   }
  #   cli::cli_inform("Computing Beveridge-Nelson decomposition with {msg}")
  # }

  return(.beveridge_nelson(ts_data))
}

#' Beveridge-Nelson decomposition via state space
#' @noRd
.beveridge_nelson <- function(ts_data) {
  # Use the mFilter package if available
  if (requireNamespace("mFilter", quietly = TRUE)) {
    bn_decomp <- mFilter::bkfilter(ts_data, pl = 2, pu = 32, type = "fixed")
    return(ts_data - bn_decomp$cycle) # Return trend
  }

  # Otherwise fall back to manual implementation
  return(.beveridge_nelson_old(ts_data))
}

#' Beveridge-Nelson decomposition
#' @noRd
.beveridge_nelson_old <- function(ts_data, ar_order = NULL) {
  # Convert to numeric vector
  y <- as.numeric(ts_data)
  n <- length(y)

  # First differences
  dy <- diff(y)

  # Estimate AR model for first differences if order not specified
  if (is.null(ar_order)) {
    # Use AIC to select optimal order (max 8 for economic data)
    max_order <- min(8, floor(length(dy) / 4))
    if (max_order < 1) {
      ar_order <- 1
    } else {
      aic_values <- numeric(max_order)
      for (i in 1:max_order) {
        tryCatch(
          {
            ar_fit <- stats::arima(dy, order = c(i, 0, 0), include.mean = TRUE)
            aic_values[i] <- AIC(ar_fit)
          },
          error = function(e) aic_values[i] <- Inf
        )
      }
      ar_order <- which.min(aic_values)
    }
  }

  # Fit ARIMA(p,1,0) model to levels (equivalent to AR(p) on differences)
  arima_fit <- stats::arima(y, order = c(ar_order, 1, 0), include.mean = TRUE)

  # Get residuals (innovations)
  innovations <- residuals(arima_fit)

  # Get AR coefficients (from the differenced model)
  if (ar_order > 0) {
    ar_coefs <- arima_fit$coef[1:ar_order]

    # Calculate the long-run impact (Beveridge-Nelson gain)
    # This is (1 + ψ₁ + ψ₂ + ...) where ψᵢ are MA(∞) coefficients
    # For AR(p): long-run impact = 1/(1 - φ₁ - φ₂ - ... - φₚ)
    long_run_impact <- 1 / (1 - sum(ar_coefs))
  } else {
    long_run_impact <- 1
  }

  # Calculate permanent component
  # The permanent component is the initial value plus the cumulative sum of
  # permanent innovations (long_run_impact * innovations)
  permanent_innovations <- long_run_impact * innovations
  permanent <- y[1] + c(0, cumsum(permanent_innovations))

  # Ensure same length as original series
  permanent <- permanent[1:n]

  # The transitory component
  transitory <- y - permanent

  # Return permanent component as trend
  trend_ts <- stats::ts(
    permanent,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract UCM trend
#' @noRd
.extract_ucm_trend <- function(ts_data, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing UCM local level trend")
  }

  return(.ucm_local_level(ts_data))
}

#' UCM Local Level using signal extraction
#' @noRd
.ucm_local_level <- function(ts_data) {
  # Estimate using state space form
  # Can also use forecast::ets() with model "ANN" (additive error, no trend, no seasonality)

  if (requireNamespace("forecast", quietly = TRUE)) {
    ets_fit <- forecast::ets(ts_data, model = "ANN")
    trend <- fitted(ets_fit)
    return(trend)
  } else {
    # Fall back to StructTS
    ss_fit <- stats::StructTS(ts_data, type = "level")
    return(fitted(ss_fit)[, "level"])
  }
}