#' Econometric Filtering Methods
#'
#' @description Internal functions for econometric trend extraction methods.
#' These methods are commonly used in macroeconomic analysis for business cycle
#' decomposition and trend extraction.
#'
#' @details
#' The econometric filters implemented here include:
#' - **HP Filter**: Hodrick-Prescott filter for trend extraction
#' - **Baxter-King**: Bandpass filter for isolating business cycle frequencies
#' - **Christiano-Fitzgerald**: Asymmetric bandpass filter
#' - **Hamilton**: Regression-based alternative to HP filter (Hamilton 2018)
#' - **Beveridge-Nelson**: ARIMA-based permanent-transitory decomposition
#' - **UCM**: Unobserved Components Model with local level
#'
#' @references
#' Hamilton, J. D. (2018). Why you should never use the Hodrick-Prescott filter.
#' Review of Economics and Statistics, 100(5), 831-843.
#'
#' Beveridge, S., & Nelson, C. R. (1981). A new approach to decomposition of
#' economic time series into permanent and transitory components.
#' Journal of Monetary Economics, 7(2), 151-174.
#'
#' @name econometric-filters
#' @keywords internal

#' Extract HP trend
#' @noRd
.extract_hp_trend <- function(ts_data, lambda, onesided = FALSE, .quiet) {
  # Determine filter type message
  filter_type <- if (onesided) "one-sided" else "two-sided"

  if (!.quiet) {
    cli::cli_inform("Computing HP filter ({filter_type}) with lambda = {lambda}")
  }

  # Convert to data frame as required by hpfilter package
  data_df <- as.data.frame(as.numeric(ts_data))

  # Use hp1() for one-sided or hp2() for two-sided
  if (onesided) {
    hp_result <- hpfilter::hp1(data_df, lambda = lambda)
  } else {
    hp_result <- hpfilter::hp2(data_df, lambda = lambda)
  }

  # Convert back to ts object
  trend <- stats::ts(
    hp_result[, 1], # Both hp1 and hp2 return data.frame with single column
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend)
}

#' Extract Baxter-King trend
#' @noRd
.extract_bk_trend <- function(ts_data, pl, pu, .quiet) {
  # Validate parameters
  if (pl <= 0) {
    cli::cli_abort("Lower period {.arg pl} must be positive, got {pl}")
  }
  if (pu <= pl) {
    cli::cli_abort(
      "Upper period {.arg pu} must be greater than lower period {.arg pl}.
      Got pl = {pl}, pu = {pu}"
    )
  }

  # Check minimum length requirement (rule of thumb: at least 3 * pu observations)
  n <- length(ts_data)
  min_length <- 3 * pu
  if (n < min_length && !.quiet) {
    cli::cli_warn(
      "Series length ({n}) is less than recommended minimum ({min_length}) for
      Baxter-King filter with pu = {pu}. Results may be unreliable."
    )
  }

  if (!.quiet) {
    cli::cli_inform("Computing Baxter-King filter with bands [{pl}, {pu}]")
  }

  # Use mFilter package for Baxter-King filter
  bk_result <- mFilter::bkfilter(ts_data, pl = pl, pu = pu)
  return(bk_result$trend)
}

#' Extract Christiano-Fitzgerald trend
#' @noRd
.extract_cf_trend <- function(ts_data, pl, pu, .quiet) {
  # Validate parameters
  if (pl <= 0) {
    cli::cli_abort("Lower period {.arg pl} must be positive, got {pl}")
  }
  if (pu <= pl) {
    cli::cli_abort(
      "Upper period {.arg pu} must be greater than lower period {.arg pl}.
      Got pl = {pl}, pu = {pu}"
    )
  }

  if (!.quiet) {
    cli::cli_inform(
      "Computing Christiano-Fitzgerald filter with bands [{pl}, {pu}]"
    )
  }

  # Use mFilter package for Christiano-Fitzgerald filter
  cf_result <- mFilter::cffilter(ts_data, pl = pl, pu = pu)
  return(cf_result$trend)
}

#' Extract Hamilton trend
#' @noRd
.extract_hamilton_trend <- function(ts_data, h, p, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing Hamilton filter with h = {h}, p = {p}")
  }

  # Use the manual implementation which follows Hamilton (2018) exactly
  return(.hamilton_filter(ts_data, h, p))
}

#' Get Hamilton filter parameters based on frequency
#' @noRd
.get_hamilton_params <- function(frequency, smooth_level = "medium") {
  params <- list(
    # Annual
    "1" = list(
      light = list(h = 1, p = 1),
      medium = list(h = 2, p = 1),
      heavy = list(h = 3, p = 1)
    ),
    # Semi-annual
    "2" = list(
      light = list(h = 2, p = 2),
      medium = list(h = 4, p = 2),
      heavy = list(h = 6, p = 2)
    ),
    # Quarterly
    "4" = list(
      light = list(h = 4, p = 4),
      medium = list(h = 8, p = 4),
      heavy = list(h = 12, p = 4)
    ),
    # Monthly
    "12" = list(
      light = list(h = 12, p = 12),
      medium = list(h = 24, p = 12),
      heavy = list(h = 36, p = 12)
    ),
    # Weekly
    "52" = list(
      light = list(h = 13, p = 13),    # Quarter
      medium = list(h = 26, p = 13),   # Half year
      heavy = list(h = 52, p = 13)     # Full year
    ),
    # Daily (trading days)
    "252" = list(
      light = list(h = 21, p = 21),    # Month
      medium = list(h = 63, p = 21),   # Quarter
      heavy = list(h = 126, p = 21)    # Half year
    )
  )

  freq_key <- as.character(frequency)
  if (freq_key %in% names(params)) {
    return(params[[freq_key]][[smooth_level]])
  } else {
    # Default fallback: h = 2 * frequency (one cycle ahead), p = frequency (one cycle of lags)
    return(list(h = 2 * frequency, p = frequency))
  }
}

#' Hamilton filter (manual implementation)
#'
#' Implements the Hamilton filter for trend extraction following Hamilton (2018).
#'
#' @details
#' This implementation follows James Hamilton's regression-based approach
#' as an alternative to the HP filter. The method regresses y_{t+h} on
#' y_t, y_{t-1}, ..., y_{t-p+1} and uses the fitted values as the trend.
#'
#' @references
#' Hamilton, J. D. (2018). Why you should never use the Hodrick-Prescott filter.
#' Review of Economics and Statistics, 100(5), 831-843.
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

  # Validate parameters
  if (!is.null(h) && (h < 1 || h != round(h))) {
    cli::cli_abort("{.arg h} must be a positive integer, got {h}")
  }
  if (!is.null(p) && (p < 1 || p != round(p))) {
    cli::cli_abort("{.arg p} must be a positive integer, got {p}")
  }

  # Check minimum length
  min_length <- h + p + 1
  if (n < min_length) {
    cli::cli_abort(
      "Time series too short for Hamilton filter.
      Need at least {min_length} observations (h + p + 1), got {n}"
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
  trend <- rep(NA_real_, n)

  # For observations p through n-h, we have the fitted values
  # fitted_vals[i] corresponds to the trend at position p + h + i - 1
  for (i in 1:length(fitted_vals)) {
    pos <- p + h + i - 1
    if (pos <= n) {
      trend[pos] <- fitted_vals[i]
    }
  }

  # Following Hamilton's recommendation: leave endpoints as NA
  # This is the mathematically correct approach - no extrapolation

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

#' Beveridge-Nelson decomposition via ARIMA
#' @noRd
.beveridge_nelson <- function(ts_data) {
  # The Beveridge-Nelson decomposition extracts permanent and transitory components
  # from an I(1) series using its ARIMA representation

  # For now, use the manual implementation which is based on ARIMA
  # In the future, we could use more sophisticated state-space methods
  return(.beveridge_nelson_arima(ts_data))
}

# OBS: in the future, consider using the bnfilter package for a more robust implementation
# https://kletts.github.io/bnfilter/reference/bnf.html
#' Beveridge-Nelson decomposition using ARIMA
#' @noRd
.beveridge_nelson_arima <- function(ts_data, ar_order = NULL) {
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
    ar_sum <- sum(ar_coefs)

    # Check for unit root or near-unit root
    if (abs(1 - ar_sum) < 1e-10) {
      # Near unit root - use small value to avoid division by zero
      long_run_impact <- 1 / 1e-10
    } else {
      long_run_impact <- 1 / (1 - ar_sum)
    }
  } else {
    long_run_impact <- 1
  }

  # Calculate permanent component
  # The permanent component is the initial value plus the cumulative sum of
  # permanent innovations (long_run_impact * innovations)
  permanent_innovations <- long_run_impact * innovations

  # Build permanent component carefully
  permanent <- numeric(n)
  permanent[1] <- y[1]
  if (n > 1) {
    cumsum_innov <- cumsum(permanent_innovations[-1])
    permanent[2:n] <- y[1] + cumsum_innov
  }

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
.extract_ucm_trend <- function(ts_data, type, .quiet) {
  # Validate type parameter
  valid_types <- c("level", "trend", "BSM")
  if (!type %in% valid_types) {
    cli::cli_abort(
      "UCM type must be one of {.val {valid_types}}, got {.val {type}}"
    )
  }

  # Check if BSM is requested for non-seasonal data
  freq <- stats::frequency(ts_data)
  if (type == "BSM" && freq == 1) {
    cli::cli_abort(
      "UCM type 'BSM' requires seasonal data (frequency > 1), got frequency = {freq}.
      Use 'level' or 'trend' instead for non-seasonal data."
    )
  }

  if (!.quiet) {
    type_desc <- switch(type,
      "level" = "local level (ARIMA 0,1,1)",
      "trend" = "local linear trend with time-varying slope",
      "BSM" = "Basic Structural Model with seasonal component"
    )
    cli::cli_inform("Computing UCM trend: {type_desc}")
  }

  return(.ucm_trend(ts_data, type, .quiet))
}

#' UCM trend extraction using state space models
#' @noRd
.ucm_trend <- function(ts_data, type = "level", .quiet = FALSE) {
  # Unobserved Components Model (UCM) using StructTS
  #
  # Three model types:
  # 1. "level": Local level model (simplest)
  #    y_t = μ_t + ε_t, μ_{t+1} = μ_t + η_t
  #    This is an ARIMA(0,1,1) model
  #
  # 2. "trend": Local linear trend model
  #    y_t = μ_t + ε_t, μ_{t+1} = μ_t + ν_t + ξ_t, ν_{t+1} = ν_t + ζ_t
  #    Allows for time-varying slope in the trend
  #
  # 3. "BSM": Basic Structural Model
  #    Adds seasonal component to local trend model
  #    y_t = μ_t + s_t + ε_t
  #    Requires frequency > 1

  tryCatch(
    {
      ss_fit <- stats::StructTS(ts_data, type = type)

      # Extract the appropriate trend component
      fitted_vals <- stats::fitted(ss_fit)

      # For level and trend models, extract "level" column
      # For BSM, we want level component (trend without seasonal)
      if (type %in% c("level", "trend")) {
        trend <- fitted_vals[, "level"]
      } else {
        # BSM has "level" column for trend
        trend <- fitted_vals[, "level"]
      }

      # Convert back to ts object with proper time index
      trend_ts <- stats::ts(
        trend,
        start = stats::start(ts_data),
        frequency = stats::frequency(ts_data)
      )
      return(trend_ts)
    },
    error = function(e) {
      # If StructTS fails, return simple smoothed version as fallback
      # This can happen with very short series or constant values
      if (!.quiet) {
        cli::cli_warn(
          "UCM estimation failed, using fallback smoothing: {e$message}"
        )
      }

      # Use exponential smoothing as a simpler fallback
      # forecast package is already in Imports
      ses_fit <- forecast::ses(ts_data, h = 0)
      return(fitted(ses_fit))
    }
  )
}

#' Extract Spencer trend
#' @noRd
.extract_spencer_trend <- function(ts_data, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing 15-term Spencer moving average")
  }

  return(.spencer(ts_data))
}

#' Spencer 15-term moving average filter
#' @description
#' Applies the classic 15-term Spencer moving average with linear
#' extrapolation at endpoints. The Spencer filter is a symmetric weighted
#' moving average designed to smooth economic time series while preserving
#' cubic polynomial trends.
#' @noRd
.spencer <- function(ts_data) {
  # Spencer 15-term weights (symmetric, sum to 1)
  # Classic weights: [-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3] / 320
  spencer_weights <- c(-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3) / 320

  y <- as.numeric(ts_data)
  n <- length(y)

  # Need at least 15 points for Spencer filter
  if (n < 15) {
    cli::cli_abort(
      "Spencer filter requires at least 15 observations, got {n}"
    )
  }

  # Linear extrapolation for 7 points at each end
  # Forward extrapolation: fit to last 7 points
  idx_fwd <- (n - 6):n
  fwd_fit <- stats::lm(y[idx_fwd] ~ idx_fwd)
  forecasts <- stats::predict(fwd_fit, newdata = data.frame(idx_fwd = (n + 1):(n + 7)))

  # Backward extrapolation: fit to first 7 points
  idx_back <- 1:7
  back_fit <- stats::lm(y[idx_back] ~ idx_back)
  backcasts <- stats::predict(back_fit, newdata = data.frame(idx_back = (-6):0))

  # Create extended series
  y_extended <- c(backcasts, y, forecasts)

  # Apply Spencer filter (two-sided symmetric)
  result <- stats::filter(y_extended, filter = spencer_weights, sides = 2)

  # Extract original portion (remove the 7 extended points on each side)
  spencer_result <- as.numeric(result[8:(length(result) - 7)])

  # Convert back to ts object
  trend_ts <- stats::ts(
    spencer_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend_ts)
}
