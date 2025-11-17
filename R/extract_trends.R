#' Extract trends from time series objects
#'
#' @description
#' Extract trend components from time series objects using various econometric methods.
#' Designed for monthly and quarterly economic data analysis. Returns trend components
#' as time series objects or a list of time series.
#'
#' @param ts_data A time series object (`ts`, `xts`, or `zoo`) or any object
#'   convertible via tsbox.
#' @param methods Character vector of trend methods.
#'   Options: `"hp"`, `"bk"`, `"cf"`, `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`,
#'   `"bn"`, `"ucm"`, `"hamilton"`, `"spencer"`, `"ewma"`, `"wma"`,
#'   `"triangular"`, `"kernel"`, `"kalman"`, `"median"`,
#'   `"gaussian"`.
#'   Default is `"stl"`.
#' @param window Unified window/period parameter for moving
#'   average methods (ma, wma, triangular, stl, ewma, median, gaussian). Must be positive.
#'   If NULL, uses frequency-appropriate defaults. For EWMA, specifies the window
#'   size when using TTR's optimized implementation. Cannot be used simultaneously
#'   with `smoothing` for EWMA method.
#' @param smoothing Unified smoothing parameter for smoothing
#'   methods (hp, loess, spline, ewma, kernel, kalman).
#'   For hp: use large values (1600+) or small values (0-1) that get converted.
#'   For EWMA: specifies the alpha parameter (0-1) for traditional exponential smoothing.
#'   Cannot be used simultaneously with `window` for EWMA method.
#'   For kernel: multiplier of optimal bandwidth (1.0 = optimal, <1 = less smooth, >1 = more smooth).
#'   For kalman: controls the ratio of measurement to process noise (higher = more smoothing).
#'   For others: typically 0-1 range.
#' @param band Unified band parameter for bandpass filters
#'   (bk, cf). Both values must be positive.
#'   For bk/cf: Provide as `c(low, high)` where low/high are periods in quarters, e.g., `c(6, 32)`.
#' @param align Unified alignment parameter for moving average
#'   methods (ma, wma, triangular, gaussian). Valid values: `"center"` (default, uses
#'   surrounding values), `"right"` (causal, uses past values only), `"left"` (anti-causal,
#'   uses future values only). Note: triangular only supports `"center"` and `"right"`.
#'   If NULL, uses `"center"` as default.
#' @param params Optional list of method-specific parameters for fine control:
#'   - **HP Filter**: `hp_onesided` (logical, default FALSE) - Use one-sided (real-time) filter instead of two-sided
#'   - **STL**: `stl_s_window` or `s.window` (numeric/"periodic", default "periodic") - Seasonal window,
#'     `stl_t_window` or `t.window` (numeric/NULL, default NULL) - Trend window,
#'     `stl_robust` or `robust` (logical, default FALSE) - Use robust fitting.
#'     Note: Both dot notation (`s.window`) and underscore notation (`stl_s_window`) are accepted.
#'   - **Spline**: `spline_cv` (logical/NULL) - Cross-validation method: NULL (none), TRUE (leave-one-out), FALSE (GCV)
#'   - **Polynomial**: `poly_degree` (integer, default 1), `poly_raw` (logical, default FALSE for orthogonal polynomials)
#'   - **UCM**: `ucm_type` (character, default "level") - Model type: "level", "trend", or "BSM"
#'   - **Others**: `bn_ar_order`, `hamilton_h`, `hamilton_p`,
#'     `kernel_type`, `kalman_measurement_noise`, `kalman_process_noise`,
#'     `median_endrule`, `gaussian_sigma`, `wma_weights`.
#'   - **Note**: Alignment parameters (`ma_align`, `wma_align`, `triangular_align`, `gaussian_align`)
#'     can still be passed via `params` but it's recommended to use the unified `align` parameter instead.
#' @param .quiet If `TRUE`, suppress informational messages.
#'
#' @return If single method, returns a `ts` object. If multiple methods, returns
#'   a named list of `ts` objects.
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom stats is.ts frequency start time ts fitted lm poly loess smooth.spline stl filter var HoltWinters AIC residuals
#' @importFrom hpfilter hp1 hp2
#' @importFrom tsbox ts_ts ts_df
#' @importFrom lubridate year month quarter
#' @importFrom RcppRoll roll_mean roll_median
#'
#' @details
#' This function focuses on monthly (frequency = 12) and quarterly (frequency = 4)
#' economic data. It uses established econometric methods with appropriate defaults:
#'
#' - **HP Filter**: lambda=1600 (quarterly), lambda=14400 (monthly). Supports both two-sided and one-sided (real-time) variants
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
#' - **Advanced MA**: EWMA with various implementations
#' - **Kernel Smoother**: Non-parametric regression with various kernel functions
#' - **Kalman Smoother**: Adaptive filtering for noisy time series
#' - **Median Filter**: Robust filtering using running medians to remove outliers
#' - **Gaussian Filter**: Weighted average with Gaussian (normal) density weights
#'
#' **Parameter Usage Notes**:
#' - **HP Filter**: Use `hp_onesided=TRUE` for real-time analysis or when future data should not
#'   influence current estimates. One-sided filter is appropriate for nowcasting, policy analysis,
#'   and avoiding look-ahead bias. Default two-sided filter is optimal for historical analysis.
#' - **EWMA**: Use either `window` (TTR optimization) OR `smoothing` (alpha parameter), not both
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
#'   methods = c("ma", "wma", "triangular"),
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
#' # Moving average with right alignment (causal filter)
#' ma_causal <- extract_trends(
#'   AirPassengers,
#'   methods = "ma",
#'   window = 12,
#'   align = "right"
#' )
#'
#' # Signal processing methods with specific parameters
#' finance_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("kalman", "gaussian"),
#'   window = 9,  # For Gaussian filter
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
#' # HP Filter: One-sided (real-time) vs Two-sided (historical)
#' hp_realtime <- extract_trends(
#'   AirPassengers,
#'   methods = "hp",
#'   params = list(hp_onesided = TRUE)  # For nowcasting and real-time analysis
#' )
#'
#' # STL with custom parameters via params (both notations work)
#' stl_custom1 <- extract_trends(
#'   AirPassengers,
#'   methods = "stl",
#'   params = list(s.window = 21, robust = TRUE)  # Dot notation
#' )
#'
#' stl_custom2 <- extract_trends(
#'   AirPassengers,
#'   methods = "stl",
#'   params = list(stl_s_window = 21, stl_robust = TRUE)  # Underscore notation
#' )
#'
#' # Advanced: fine-tune specific methods
#' custom_trends <- extract_trends(
#'   AirPassengers,
#'   methods = c("median", "kalman"),
#'   window = 7,
#'   params = list(median_endrule = "constant")
#' )
#'
#' @export
extract_trends <- function(
  ts_data,
  methods = "stl",
  window = NULL,
  smoothing = NULL,
  band = NULL,
  align = NULL,
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
    "spencer",
    "ewma",
    "wma",
    "triangular",
    "kernel",
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

  if (!is.null(align)) {
    if (!is.character(align) || length(align) != 1) {
      cli::cli_abort("{.arg align} must be a single character value")
    }
    if (!align %in% c("left", "center", "right")) {
      cli::cli_abort(
        "{.arg align} must be one of 'left', 'center', or 'right', got {.val {align}}"
      )
    }
  }

  if (!is.list(params)) {
    cli::cli_abort("{.arg params} must be a list")
  }

  # Validate frequency
  freq <- stats::frequency(ts_data)
  if (freq < 1 || freq > 365) {
    cli::cli_abort(
      "Frequency must be between 1 (annual) and 365 (daily), got {freq}"
    )
  }

  # Warn for frequency-sensitive methods with non-standard frequencies
  if (!freq %in% c(1, 4, 12) && any(methods %in% c("hp", "bk", "cf", "hamilton"))) {
    freq_sensitive <- intersect(methods, c("hp", "bk", "cf", "hamilton"))
    if (!.quiet) {
      cli::cli_warn(
        "Methods {.val {freq_sensitive}} are optimized for standard economic frequencies.
         Using frequency = {freq} with default parameters may produce suboptimal results.
         Consider specifying parameters explicitly via the {.arg params} argument."
      )
    }
  }

  # Check for STL with non-seasonal data
  if ("stl" %in% methods && freq == 1) {
    if (!.quiet) {
      cli::cli_inform(
        "STL requires seasonal data (frequency > 1). Will use HP filter fallback for non-seasonal data."
      )
    }
  }

  # Check minimum observations
  min_obs <- 3 * freq
  if (length(ts_data) < min_obs && !.quiet) {
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
    align = align,
    params = params,
    frequency = freq,
    .quiet = .quiet
  )

  # Extract parameters from unified system with defaults
  .get_param <- function(name, default) unified_params[[name]] %||% default

  # Method-specific parameters
  hp_lambda <- .get_param("hp_lambda", if (freq == 4) 1600 else 14400)
  hp_onesided <- .get_param("hp_onesided", FALSE)
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
  ewma_window <- .get_param("ewma_window", NULL)
  ewma_alpha <- .get_param("ewma_alpha", NULL)
  wma_window <- .get_param("wma_window", freq)
  wma_weights <- .get_param("wma_weights", NULL)
  wma_align <- .get_param("wma_align", "center")
  triangular_window <- .get_param("triangular_window", freq)
  triangular_align <- .get_param("triangular_align", "center")
  bk_low <- .get_param("bk_low", 6)
  bk_high <- .get_param("bk_high", 32)
  cf_low <- .get_param("cf_low", 6)
  cf_high <- .get_param("cf_high", 32)
  kernel_bandwidth <- .get_param("kernel_bandwidth", NULL)
  kernel_type <- .get_param("kernel_type", "normal")
  kalman_measurement_noise <- .get_param("kalman_measurement_noise", NULL)
  kalman_process_noise <- .get_param("kalman_process_noise", NULL)
  median_window <- .get_param("median_window", 5)
  median_endrule <- .get_param("median_endrule", "median")
  gaussian_window <- .get_param("gaussian_window", 7)
  gaussian_sigma <- .get_param("gaussian_sigma", NULL)
  gaussian_align <- .get_param("gaussian_align", "center")

  # Extract trends
  trends <- list()

  for (method in methods) {
    trend <- switch(
      method,
      "hp" = .extract_hp_trend(ts_data, hp_lambda, hp_onesided, .quiet),
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
      "spencer" = .extract_spencer_trend(ts_data, .quiet),
      "ewma" = .extract_ewma_trend(ts_data, ewma_window, ewma_alpha, .quiet),
      "wma" = .extract_wma_trend(ts_data, wma_window, wma_weights, wma_align, .quiet),
      "triangular" = .extract_triangular_trend(ts_data, triangular_window, triangular_align, .quiet),
      "kernel" = .extract_kernel_trend(
        ts_data,
        kernel_bandwidth,
        kernel_type,
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

