#' Extract trends from time series objects
#'
#' @description
#' Extract trend components from time series objects using various econometric methods.
#' Designed for monthly and quarterly economic data analysis. Returns trend components
#' as time series objects or a list of time series.
#'
#' @param ts_data A time series object (`ts`, `xts`, or `zoo`) or any object
#'   convertible via tsbox.
#' @param methods `[character()]` Character vector of trend methods.
#'   Options: `"hp"`, `"bk"`, `"cf"`, `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`,
#'   `"bn"`, `"ucm"`, `"hamilton"`, `"exp_simple"`, `"exp_double"`, `"ewma"`, `"wma"`,
#'   `"zlema"`, `"triangular"`, `"sg"`, `"kernel"`, `"butter"`, `"kalman"`, `"median"`,
#'   `"gaussian"`.
#'   Default is `"hp"`.
#' @param window `[numeric(1)] | NULL` Unified window/period parameter for moving
#'   average methods (ma, wma, zlema, triangular, stl, sg, ewma, median, gaussian). Must be positive.
#'   If NULL, uses frequency-appropriate defaults. For EWMA, specifies the window
#'   size when using TTR's optimized implementation. Cannot be used simultaneously
#'   with `smoothing` for EWMA method.
#' @param smoothing `[numeric(1)] | NULL` Unified smoothing parameter for smoothing
#'   methods (hp, loess, spline, exp_*, ewma, kernel, kalman).
#'   For hp: use large values (1600+) or small values (0-1) that get converted.
#'   For EWMA: specifies the alpha parameter (0-1) for traditional exponential smoothing.
#'   Cannot be used simultaneously with `window` for EWMA method.
#'   For kernel: multiplier of optimal bandwidth (1.0 = optimal, <1 = less smooth, >1 = more smooth).
#'   For kalman: controls the ratio of measurement to process noise (higher = more smoothing).
#'   For others: typically 0-1 range.
#' @param band `[numeric(2)] | NULL` Unified band parameter for bandpass filters
#'   (bk, cf, butter). Both values must be positive.
#'   For bk/cf: Provide as `c(low, high)` where low/high are periods in quarters, e.g., `c(6, 32)`.
#'   For butter: Provide as `c(cutoff, order)` where cutoff is normalized frequency (0-1) and order is integer, e.g., `c(0.1, 2)`.
#' @param params `[list()]` Optional list of method-specific parameters for fine control:
#'   - **Spline**: `spline_cv` (logical/NULL) - Cross-validation method: NULL (none), TRUE (leave-one-out), FALSE (GCV)
#'   - **Polynomial**: `poly_degree` (integer, default 1), `poly_raw` (logical, default FALSE for orthogonal polynomials)
#'   - **UCM**: `ucm_type` (character, default "level") - Model type: "level", "trend", or "BSM"
#'   - **Others**: `exp_beta`, `bn_ar_order`, `hamilton_h`, `hamilton_p`, `sg_poly_order`,
#'     `kernel_type`, `butter_type`, `kalman_measurement_noise`, `kalman_process_noise`,
#'     `median_endrule`, `gaussian_sigma`, `gaussian_align`, `ma_align`, `wma_weights`,
#'     `wma_align`, `zlema_ratio`, `triangular_align`.
#' @param .quiet `[logical(1)]` If `TRUE`, suppress informational messages.
#'
#' @return If single method, returns a `ts` object. If multiple methods, returns
#'   a named list of `ts` objects.
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom stats is.ts frequency start time ts fitted lm poly loess smooth.spline stl filter var HoltWinters AIC residuals
#' @importFrom hpfilter hp2
#' @importFrom tsbox ts_ts
#' @importFrom zoo as.Date.ts coredata
#' @importFrom lubridate year month quarter
#' @importFrom TTR SMA EMA DEMA HMA ALMA
#' @importFrom forecast ses holt
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
#' - **Beveridge-Nelson**: Permanent/transitory decomposition
#' - **UCM**: Unobserved Components Model (local level)
#' - **Hamilton**: Regression-based alternative to HP filter
#' - **Exponential Smoothing**: Simple and double exponential smoothing
#' - **Advanced MA**: EWMA, ALMA, DEMA, HMA variations
#' - **Savitzky-Golay**: Polynomial smoothing that preserves peaks and valleys
#' - **Kernel Smoother**: Non-parametric regression with various kernel functions
#' - **Butterworth**: Clean frequency domain low-pass filtering
#' - **Kalman Smoother**: Adaptive filtering for noisy time series
#' - **Median Filter**: Robust filtering using running medians to remove outliers
#' - **Gaussian Filter**: Weighted average with Gaussian (normal) density weights
#'
#' **Parameter Usage Notes**:
#' - **EWMA**: Use either `window` (TTR optimization) OR `smoothing` (alpha parameter), not both
#' - **Butterworth**: The `band` parameter expects `c(cutoff, order)` where cutoff is 0-1 normalized frequency
#' - **Kalman**: Use `smoothing` parameter or `params` list for fine control of noise parameters
#' - **Spline**: Use `spline_cv` to control cross-validation (NULL=none, TRUE=LOO-CV, FALSE=GCV)
#' - **Polynomial**: Use `poly_raw=FALSE` for orthogonal polynomials (more stable for degree > 2)
#'   or `poly_raw=TRUE` for raw polynomials. Warning issued for degree > 3 (overfitting risk).
#' - **UCM**: Choose model type - "level" (simplest), "trend" (time-varying slope), or
#'   "BSM" (with seasonal component, requires seasonal data)
#'
#' @examples
#' # Single method
#' hp_trend <- extract_trends(AirPassengers, methods = "hp")
#'
#' # Multiple methods with unified smoothing
#' smooth_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("hp", "loess", "ewma"),
#'   smoothing = 0.3
#' )
#'
#' # EWMA with window (uses TTR optimization)
#' ewma_window <- extract_trends(AirPassengers, methods = "ewma", window = 12)
#'
#' # EWMA with alpha (traditional formula)
#' ewma_alpha <- extract_trends(AirPassengers, methods = "ewma", smoothing = 0.2)
#'
#' # Moving averages with unified window
#' ma_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("ma", "wma", "zlema", "triangular"),
#'   window = 8
#' )
#'
#' # Bandpass filters with unified band
#' bp_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("bk", "cf"),
#'   band = c(6, 32)
#' )
#'
#' # Signal processing methods with specific parameters
#' finance_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("sg", "kalman", "butter"),
#'   window = 9,  # For Savitzky-Golay
#'   band = c(0.05, 2),  # Butterworth cutoff and order
#'   params = list(kalman_measurement_noise = 0.1)  # Kalman-specific parameter
#' )
#'
#' # Spline with cross-validation options
#' spline_trends <- extract_trends(
#'   AirPassengers,
#'   methods = "spline",
#'   params = list(spline_cv = FALSE)  # Use GCV instead of default
#' )
#'
#' # Polynomial with orthogonal vs raw polynomials
#' poly_trends <- extract_trends(
#'   AirPassengers,
#'   methods = "poly",
#'   params = list(poly_degree = 2, poly_raw = FALSE)  # Orthogonal (default)
#' )
#'
#' # UCM with different model types
#' ucm_trends <- extract_trends(
#'   AirPassengers,
#'   methods = "ucm",
#'   params = list(ucm_type = "BSM")  # Basic Structural Model with seasonality
#' )
#'
#' # Advanced: fine-tune specific methods
#' custom_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("sg", "kalman"),
#'   window = 7,
#'   params = list(sg_poly_order = 3)
#' )
#'
#' @export
extract_trends <- function(
  ts_data,
  methods = "hp",
  window = NULL,
  smoothing = NULL,
  band = NULL,
  params = list(),
  .quiet = FALSE
) {
  # Input validation
  if (is.null(ts_data)) {
    cli::cli_abort("{.arg ts_data} cannot be NULL")
  }

  # Validate methods
  valid_methods <- c(
    "hp",
    "bk",
    "cf",
    "ma",
    "stl",
    "loess",
    "spline",
    "poly",
    "bn",
    "ucm",
    "hamilton",
    "exp_simple",
    "exp_double",
    "ewma",
    "wma",
    "zlema",
    "triangular",
    "sg",
    "kernel",
    "butter",
    "kalman",
    "median",
    "gaussian"
  )
  invalid_methods <- setdiff(methods, valid_methods)
  if (length(invalid_methods) > 0) {
    cli::cli_abort(
      "Invalid methods: {.val {invalid_methods}}.
       Valid options: {.val {valid_methods}}"
    )
  }

  # Convert to ts object using tsbox if needed
  if (!stats::is.ts(ts_data)) {
    tryCatch(
      {
        ts_data <- tsbox::ts_ts(ts_data)
      },
      error = function(e) {
        cli::cli_abort(
          "Failed to convert input to time series object.",
          "i" = "Input must be convertible to ts via tsbox package.",
          "x" = "Error: {e$message}"
        )
      }
    )
  }

  # Validate unified parameters
  if (
    !is.null(window) &&
      (!is.numeric(window) || length(window) != 1 || window <= 0)
  ) {
    cli::cli_abort("{.arg window} must be a positive numeric value")
  }

  if (
    !is.null(smoothing) && (!is.numeric(smoothing) || length(smoothing) != 1)
  ) {
    cli::cli_abort("{.arg smoothing} must be a single numeric value")
  }

  if (
    !is.null(band) && (!is.numeric(band) || length(band) != 2 || any(band <= 0))
  ) {
    cli::cli_abort(
      "{.arg band} must be a numeric vector of length 2 with positive values"
    )
  }

  if (!is.list(params)) {
    cli::cli_abort("{.arg params} must be a list")
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

  # Process unified parameters to get method-specific parameters
  unified_params <- .process_unified_params(
    methods = methods,
    window = window,
    smoothing = smoothing,
    band = band,
    params = params,
    frequency = freq
  )

  # Extract parameters from unified system with defaults
  .get_param <- function(name, default) unified_params[[name]] %||% default

  # Method-specific parameters
  hp_lambda <- .get_param("hp_lambda", if (freq == 4) 1600 else 14400)
  ma_window <- .get_param("ma_window", freq)
  ma_align <- .get_param("ma_align", "center")
  stl_s_window <- .get_param("stl_s_window", "periodic")
  loess_span <- .get_param("loess_span", 0.75)
  spline_spar <- .get_param("spline_spar", NULL)
  spline_cv <- .get_param("spline_cv", NULL)
  poly_degree <- .get_param("poly_degree", 1)
  poly_raw <- .get_param("poly_raw", FALSE)
  bn_ar_order <- .get_param("bn_ar_order", NULL)
  ucm_type <- .get_param("ucm_type", "level")
  hamilton_h <- .get_param("hamilton_h", 8)
  hamilton_p <- .get_param("hamilton_p", 4)
  exp_alpha <- .get_param("exp_alpha", NULL)
  exp_beta <- .get_param("exp_beta", NULL)
  ewma_window <- .get_param("ewma_window", NULL)
  ewma_alpha <- .get_param("ewma_alpha", NULL)
  wma_window <- .get_param("wma_window", freq)
  wma_weights <- .get_param("wma_weights", NULL)
  wma_align <- .get_param("wma_align", "center")
  zlema_window <- .get_param("zlema_window", 10)
  zlema_ratio <- .get_param("zlema_ratio", NULL)
  triangular_window <- .get_param("triangular_window", freq)
  triangular_align <- .get_param("triangular_align", "center")
  bk_low <- .get_param("bk_low", 6)
  bk_high <- .get_param("bk_high", 32)
  cf_low <- .get_param("cf_low", 6)
  cf_high <- .get_param("cf_high", 32)
  sg_window <- .get_param("sg_window", 7)
  sg_poly_order <- .get_param("sg_poly_order", 2)
  kernel_bandwidth <- .get_param("kernel_bandwidth", NULL)
  kernel_type <- .get_param("kernel_type", "normal")
  butter_cutoff <- .get_param("butter_cutoff", 0.1)
  butter_order <- .get_param("butter_order", 2)
  kalman_measurement_noise <- .get_param("kalman_measurement_noise", NULL)
  kalman_process_noise <- .get_param("kalman_process_noise", NULL)
  median_window <- .get_param("median_window", 5)
  median_endrule <- .get_param("median_endrule", "median")
  gaussian_window <- .get_param("gaussian_window", 7)
  gaussian_sigma <- .get_param("gaussian_sigma", NULL)
  gaussian_align <- .get_param("gaussian_align", "center")

  # Validate methods
  valid_methods <- c(
    "hp",
    "bk",
    "cf",
    "ma",
    "stl",
    "loess",
    "spline",
    "poly",
    "bn",
    "ucm",
    "hamilton",
    "exp_simple",
    "exp_double",
    "ewma",
    "wma",
    "zlema",
    "triangular",
    "sg",
    "kernel",
    "butter",
    "kalman",
    "median",
    "gaussian"
  )
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
    trend <- switch(
      method,
      "hp" = .extract_hp_trend(ts_data, hp_lambda, .quiet),
      "bk" = .extract_bk_trend(ts_data, bk_low, bk_high, .quiet),
      "cf" = .extract_cf_trend(ts_data, cf_low, cf_high, .quiet),
      "ma" = .extract_ma_trend(ts_data, ma_window, ma_align, .quiet),
      "stl" = .extract_stl_trend(ts_data, stl_s_window, .quiet),
      "loess" = .extract_loess_trend(ts_data, loess_span, .quiet),
      "spline" = .extract_spline_trend(ts_data, spline_spar, spline_cv, .quiet),
      "poly" = .extract_poly_trend(ts_data, poly_degree, poly_raw, .quiet),
      "bn" = .extract_bn_trend(ts_data, .quiet),
      "ucm" = .extract_ucm_trend(ts_data, ucm_type, .quiet),
      "hamilton" = .extract_hamilton_trend(
        ts_data,
        hamilton_h,
        hamilton_p,
        .quiet
      ),
      "exp_simple" = .extract_exp_simple_trend(ts_data, exp_alpha, .quiet),
      "exp_double" = .extract_exp_double_trend(
        ts_data,
        exp_alpha,
        exp_beta,
        .quiet
      ),
      "ewma" = .extract_ewma_trend(ts_data, ewma_window, ewma_alpha, .quiet),
      "wma" = .extract_wma_trend(ts_data, wma_window, wma_weights, wma_align, .quiet),
      "zlema" = .extract_zlema_trend(ts_data, zlema_window, zlema_ratio, .quiet),
      "triangular" = .extract_triangular_trend(ts_data, triangular_window, triangular_align, .quiet),
      "sg" = .extract_sg_trend(ts_data, sg_window, sg_poly_order, .quiet),
      "kernel" = .extract_kernel_trend(
        ts_data,
        kernel_bandwidth,
        kernel_type,
        .quiet
      ),
      "butter" = .extract_butter_trend(
        ts_data,
        butter_cutoff,
        butter_order,
        .quiet
      ),
      "kalman" = .extract_kalman_trend(
        ts_data,
        kalman_measurement_noise,
        kalman_process_noise,
        .quiet
      ),
      "median" = .extract_median_trend(
        ts_data,
        median_window,
        median_endrule,
        .quiet
      ),
      "gaussian" = .extract_gaussian_trend(
        ts_data,
        gaussian_window,
        gaussian_sigma,
        gaussian_align,
        .quiet
      )
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
    hp_result[, 1], # hp2 returns data.frame with single column
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

  # mFilter package is already in Imports
  bk_result <- mFilter::bkfilter(ts_data, pl = pl, pu = pu)
  return(bk_result$trend)
}

#' @noRd
.extract_cf_trend <- function(ts_data, pl, pu, .quiet) {
  if (!.quiet) {
    cli::cli_inform(
      "Computing Christiano-Fitzgerald filter with bands [{pl}, {pu}]"
    )
  }

  # mFilter package is already in Imports
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

#' @noRd
.extract_ucm_trend <- function(ts_data, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing UCM local level trend")
  }

  return(.ucm_local_level(ts_data))
}

#' @noRd
.extract_hamilton_trend <- function(ts_data, h, p, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing Hamilton filter with h = {h}, p = {p}")
  }

  return(.hamilton_filter(ts_data, h, p))
}

#' @noRd
.extract_exp_simple_trend <- function(ts_data, alpha, .quiet) {
  if (!.quiet) {
    msg <- if (is.null(alpha)) "optimized alpha" else "alpha = {alpha}"
    cli::cli_inform("Computing simple exponential smoothing with {msg}")
  }

  return(.exp_smoothing_simple(ts_data, alpha))
}

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

#' @noRd
.extract_ewma_trend <- function(ts_data, window, alpha, .quiet) {
  if (!.quiet) {
    if (!is.null(window)) {
      cli::cli_inform("Computing EWMA with window = {window}")
    } else if (!is.null(alpha)) {
      cli::cli_inform("Computing EWMA with alpha = {alpha}")
    } else {
      cli::cli_inform("Computing EWMA with default alpha = 0.1")
    }
  }

  return(.ewma(ts_data, window, alpha))
}


#' @noRd
.extract_sg_trend <- function(ts_data, window, poly_order, .quiet) {
  if (!.quiet) {
    cli::cli_inform(
      "Computing Savitzky-Golay filter with window = {window}, polynomial order = {poly_order}"
    )
  }

  return(.savitzky_golay(ts_data, window, poly_order))
}

#' @noRd
.extract_kernel_trend <- function(ts_data, bandwidth, kernel_type, .quiet) {
  if (!.quiet) {
    bandwidth_msg <- if (is.null(bandwidth)) "auto" else "{bandwidth}"
    cli::cli_inform(
      "Computing kernel smoother with bandwidth = {bandwidth_msg}, kernel = {kernel_type}"
    )
  }

  return(.kernel_smooth(ts_data, bandwidth, kernel_type))
}

#' @noRd
.extract_butter_trend <- function(ts_data, cutoff, order, .quiet) {
  if (!.quiet) {
    cli::cli_inform(
      "Computing Butterworth filter with cutoff = {cutoff}, order = {order}"
    )
  }

  return(.butterworth_filter(ts_data, cutoff, order))
}

#' @noRd
.extract_kalman_trend <- function(
  ts_data,
  measurement_noise,
  process_noise,
  .quiet
) {
  if (!.quiet) {
    noise_msg <- if (is.null(measurement_noise)) {
      "auto"
    } else {
      "{measurement_noise}"
    }
    cli::cli_inform(
      "Computing Kalman smoother with measurement noise = {noise_msg}"
    )
  }

  return(.kalman_smooth(ts_data, measurement_noise, process_noise))
}
