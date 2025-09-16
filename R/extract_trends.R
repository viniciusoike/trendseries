#' Extract trends from time series objects
#'
#' @description
#' Extract trend components from time series objects using various econometric methods.
#' Designed for monthly and quarterly economic data analysis. Returns trend components
#' as time series objects or a list of time series.
#'
#' @param ts_data A time series object (`ts`, `xts`, or `zoo`)
#' @param methods Character vector of trend methods. Options: `"hp"`, `"bk"`, `"cf"`,
#'   `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`, `"bn"`, `"ucm"`, `"hamilton"`,
#'   `"exp_simple"`, `"exp_double"`, `"ewma"`, `"alma"`, `"dema"`, `"hma"`, `"sg"`,
#'   `"kernel"`, `"butter"`, `"kalman"`, `"wavelet"`. Default is `"hp"`.
#' @param window Unified window/period parameter for moving average methods (ma, alma, dema, hma, stl, sg).
#'   If NULL, uses frequency-appropriate defaults.
#' @param smoothing Unified smoothing parameter for smoothing methods (hp, loess, spline, exp_*, ewma, kernel, kalman, wavelet).
#'   For hp: use large values (1600+) or small values (0-1) that get converted.
#'   For others: typically 0-1 range.
#' @param band Unified band parameter for bandpass filters (bk, cf, butter).
#'   For bk/cf: Provide as c(low, high), e.g., c(6, 32).
#'   For butter: Provide as c(cutoff, order), e.g., c(0.1, 2).
#' @param params Optional list of method-specific parameters for fine control:
#'   alma_offset, alma_sigma, exp_beta, poly_degree, bn_ar_order, hamilton_h, hamilton_p,
#'   sg_poly_order, kernel_type, butter_type, kalman_measurement_noise, kalman_process_noise, wavelet_type.
#' @param .quiet If TRUE, suppress informational messages.
#'
#' @return If single method, returns a `ts` object. If multiple methods, returns
#'   a named list of `ts` objects.
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom stats is.ts frequency start time ts fitted lm poly loess smooth.spline stl filter var HoltWinters
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
#' - **Wavelet Denoising**: Multi-resolution analysis and noise removal
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
#' # Moving averages with unified window
#' ma_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("ma", "dema", "hma"),
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
#' # New financial/economic methods
#' finance_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("sg", "kalman", "butter"),
#'   window = 9,  # For Savitzky-Golay
#'   band = c(0.05, 2),  # Butterworth cutoff and order
#'   smoothing = 0.1  # Kalman noise ratio
#' )
#'
#' # Advanced: fine-tune specific methods
#' custom_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("sg", "wavelet"),
#'   window = 7,
#'   params = list(sg_poly_order = 3, wavelet_type = "db4")
#' )
#'
#' @export
extract_trends <- function(ts_data,
                          methods = "hp",
                          window = NULL,
                          smoothing = NULL,
                          band = NULL,
                          params = list(),
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
  stl_s_window <- .get_param("stl_s_window", "periodic")
  loess_span <- .get_param("loess_span", 0.75)
  spline_spar <- .get_param("spline_spar", NULL)
  poly_degree <- .get_param("poly_degree", 1)
  bn_ar_order <- .get_param("bn_ar_order", NULL)
  hamilton_h <- .get_param("hamilton_h", 8)
  hamilton_p <- .get_param("hamilton_p", 4)
  exp_alpha <- .get_param("exp_alpha", NULL)
  exp_beta <- .get_param("exp_beta", NULL)
  ewma_alpha <- .get_param("ewma_alpha", 0.1)
  alma_window <- .get_param("alma_window", 9)
  alma_offset <- .get_param("alma_offset", 0.85)
  alma_sigma <- .get_param("alma_sigma", 6)
  dema_period <- .get_param("dema_period", 14)
  hma_period <- .get_param("hma_period", 14)
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
  wavelet_threshold <- .get_param("wavelet_threshold", NULL)
  wavelet_type <- .get_param("wavelet_type", "haar")

  # Validate methods
  valid_methods <- c("hp", "bk", "cf", "ma", "stl", "loess", "spline", "poly",
                     "bn", "ucm", "hamilton", "exp_simple", "exp_double",
                     "ewma", "alma", "dema", "hma", "sg", "kernel", "butter",
                     "kalman", "wavelet")
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
      "poly" = .extract_poly_trend(ts_data, poly_degree, .quiet),
      "bn" = .extract_bn_trend(ts_data, bn_ar_order, .quiet),
      "ucm" = .extract_ucm_trend(ts_data, .quiet),
      "hamilton" = .extract_hamilton_trend(ts_data, hamilton_h, hamilton_p, .quiet),
      "exp_simple" = .extract_exp_simple_trend(ts_data, exp_alpha, .quiet),
      "exp_double" = .extract_exp_double_trend(ts_data, exp_alpha, exp_beta, .quiet),
      "ewma" = .extract_ewma_trend(ts_data, ewma_alpha, .quiet),
      "alma" = .extract_alma_trend(ts_data, alma_window, alma_offset, alma_sigma, .quiet),
      "dema" = .extract_dema_trend(ts_data, dema_period, .quiet),
      "hma" = .extract_hma_trend(ts_data, hma_period, .quiet),
      "sg" = .extract_sg_trend(ts_data, sg_window, sg_poly_order, .quiet),
      "kernel" = .extract_kernel_trend(ts_data, kernel_bandwidth, kernel_type, .quiet),
      "butter" = .extract_butter_trend(ts_data, butter_cutoff, butter_order, .quiet),
      "kalman" = .extract_kalman_trend(ts_data, kalman_measurement_noise, kalman_process_noise, .quiet),
      "wavelet" = .extract_wavelet_trend(ts_data, wavelet_threshold, wavelet_type, .quiet)
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

  # Use TTR's optimized SMA implementation (C code)
  ma_result <- TTR::SMA(as.numeric(ts_data), n = window)

  # Handle NAs at the beginning by using original values
  if (any(is.na(ma_result))) {
    na_count <- sum(is.na(ma_result))
    ma_result[1:na_count] <- as.numeric(ts_data)[1:na_count]
  }

  # Convert back to ts object
  trend <- stats::ts(ma_result, start = stats::start(ts_data), frequency = stats::frequency(ts_data))
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

#' @noRd
.extract_bn_trend <- function(ts_data, ar_order, .quiet) {
  if (!.quiet) {
    msg <- if (is.null(ar_order)) "automatic AR order selection" else "AR({ar_order})"
    cli::cli_inform("Computing Beveridge-Nelson decomposition with {msg}")
  }

  return(.beveridge_nelson(ts_data, ar_order))
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
    cli::cli_inform("Computing double exponential smoothing with alpha = {alpha_msg}, beta = {beta_msg}")
  }

  return(.exp_smoothing_double(ts_data, alpha, beta))
}

#' @noRd
.extract_ewma_trend <- function(ts_data, alpha, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing EWMA with alpha = {alpha}")
  }

  return(.ewma(ts_data, alpha))
}

#' @noRd
.extract_alma_trend <- function(ts_data, window, offset, sigma, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing ALMA with window = {window}, offset = {offset}, sigma = {sigma}")
  }

  return(.alma(ts_data, window, offset, sigma))
}

#' @noRd
.extract_dema_trend <- function(ts_data, period, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing DEMA with period = {period}")
  }

  return(.dema(ts_data, period))
}

#' @noRd
.extract_hma_trend <- function(ts_data, period, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing HMA with period = {period}")
  }

  return(.hma(ts_data, period))
}

#' @noRd
.extract_sg_trend <- function(ts_data, window, poly_order, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing Savitzky-Golay filter with window = {window}, polynomial order = {poly_order}")
  }

  return(.savitzky_golay(ts_data, window, poly_order))
}

#' @noRd
.extract_kernel_trend <- function(ts_data, bandwidth, kernel_type, .quiet) {
  if (!.quiet) {
    bandwidth_msg <- if (is.null(bandwidth)) "auto" else "{bandwidth}"
    cli::cli_inform("Computing kernel smoother with bandwidth = {bandwidth_msg}, kernel = {kernel_type}")
  }

  return(.kernel_smooth(ts_data, bandwidth, kernel_type))
}

#' @noRd
.extract_butter_trend <- function(ts_data, cutoff, order, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing Butterworth filter with cutoff = {cutoff}, order = {order}")
  }

  return(.butterworth_filter(ts_data, cutoff, order))
}

#' @noRd
.extract_kalman_trend <- function(ts_data, measurement_noise, process_noise, .quiet) {
  if (!.quiet) {
    noise_msg <- if (is.null(measurement_noise)) "auto" else "{measurement_noise}"
    cli::cli_inform("Computing Kalman smoother with measurement noise = {noise_msg}")
  }

  return(.kalman_smooth(ts_data, measurement_noise, process_noise))
}

#' @noRd
.extract_wavelet_trend <- function(ts_data, threshold, wavelet_type, .quiet) {
  if (!.quiet) {
    threshold_msg <- if (is.null(threshold)) "auto" else "{threshold}"
    cli::cli_inform("Computing wavelet denoising with threshold = {threshold_msg}, wavelet = {wavelet_type}")
  }

  return(.wavelet_denoise(ts_data, threshold, wavelet_type))
}