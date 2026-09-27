#' Method registry --------------------------------------------------------

#' Canonical registry of trend extraction methods
#'
#' @description Single source of truth for the methods supported by
#' [augment_trends()] and [extract_trends()]. The validation routines (via
#' `.valid_methods()`) read from this table, so adding a method here propagates
#' everywhere. The *Trend Extraction Methods* vignette and the README render
#' their method tables from it.
#'
#' `one_sided` records whether the trend at time t uses only observations up
#' to t: `"always"`, `"option"` (via `align = "right"` or `hp_onesided`), or
#' `"no"`.
#' @return A data frame with columns `method`, `category`, `description`,
#'   `typical_use`, and `one_sided`.
#' @noRd
.method_info <- function() {
  # fmt: skip
  rows <- c(
    "hp",         "econometric",    "Hodrick-Prescott filter",                "General-purpose business-cycle trend",       "option",
    "bk",         "bandpass",       "Baxter-King bandpass filter",            "Remove a band of cycle frequencies",         "no",
    "cf",         "bandpass",       "Christiano-Fitzgerald bandpass filter",  "Band removal that keeps the endpoints",      "no",
    "ma",         "moving_average", "Simple moving average",                  "Quick, intuitive smoothing",                 "option",
    "stl",        "smoothing",      "Seasonal-trend decomposition via Loess", "Trend of a strongly seasonal series",        "no",
    "loess",      "smoothing",      "Local polynomial regression (loess)",    "Flexible non-parametric trend",              "no",
    "spline",     "smoothing",      "Smoothing splines",                      "Smooth curve, GCV penalty by default",       "no",
    "poly",       "smoothing",      "Polynomial trend",                       "Simple global trend shape",                  "no",
    "bn",         "econometric",    "Beveridge-Nelson decomposition",         "Permanent component of an I(1) series",      "no",
    "ucm",        "econometric",    "Unobserved components model",            "Model-based, stochastic trend",              "no",
    "hamilton",   "econometric",    "Hamilton regression filter",             "Regression-based alternative to HP",         "no",
    "spencer",    "moving_average", "Spencer's 15-term moving average",       "Classic actuarial graduation",               "no",
    "ewma",       "moving_average", "Exponentially weighted moving average",  "Real-time, recent points weigh more",        "always",
    "wma",        "moving_average", "Weighted moving average",                "Smoothing with custom weights",              "option",
    "triangular", "moving_average", "Triangular moving average",              "Smoother than a simple moving average",      "option",
    "kernel",     "smoothing",      "Kernel smoother",                        "Non-parametric, bandwidth-controlled",       "no",
    "kalman",     "smoothing",      "Kalman filter/smoother",                 "Local-level trend of a noisy series",        "no",
    "median",     "moving_average", "Median filter",                          "Smoothing robust to outliers and spikes",    "no",
    "gaussian",   "moving_average", "Gaussian-weighted moving average",       "Smooth, bell-weighted average",              "option",
    "henderson",  "moving_average", "Henderson moving average",               "Trend filter used inside X-11",              "no"
  )

  columns <- c("method", "category", "description", "typical_use", "one_sided")
  info <- as.data.frame(
    matrix(rows, ncol = length(columns), byrow = TRUE),
    stringsAsFactors = FALSE
  )
  names(info) <- columns

  return(info)
}

#' Unified parameters accepted by each trend method
#'
#' @description Derives, from the routing vectors in `R/utils.R`, which unified
#' parameters each method receives. Because `.map_unified_params()` routes by
#' the same vectors, this table cannot disagree with the code. The
#' *Trend Extraction Methods* vignette renders it.
#' @return A data frame with one row per method (in `.method_info()` order)
#'   and logical columns `window`, `window_vector`, `smoothing`, `band`, and
#'   `align`.
#' @noRd
.method_params <- function() {
  methods <- .valid_methods()

  params <- data.frame(
    method = methods,
    window = methods %in% .WINDOW_METHODS,
    window_vector = methods %in% .WINDOW_VECTOR_METHODS,
    smoothing = methods %in% .SMOOTHING_METHODS,
    band = methods %in% .BAND_METHODS,
    align = methods %in% .ALIGN_METHODS,
    stringsAsFactors = FALSE
  )

  return(params)
}

#' Canonical vector of valid method names
#'
#' @description Returns the method names supported by [augment_trends()] and
#' [extract_trends()] in their canonical order. Used by input validation.
#' @noRd
.valid_methods <- function() {
  return(.method_info()$method)
}

#' Canonical vector of decomposition method names
#'
#' @description Returns the method names supported by [decompose_series()] in
#' their canonical order. Used by input validation.
#' @noRd
.decompose_methods <- function() {
  return(c("stl", "regression", "classic", "bsm", "seats"))
}

#' Rolling aggregation registry ---------------------------------------------

#' Canonical registry of rolling aggregation statistics
#'
#' @description Single source of truth for the statistics supported by
#' [augment_rolling()] and [roll_series()]. These are aggregations, not trend
#' estimators: they are deliberately kept out of `.method_info()` so they never
#' reach [detrend_series()], which would subtract them from the series.
#' @return A data frame with columns `stat` and `description`.
#' @noRd
.rolling_info <- function() {
  data.frame(
    stat = c("sum", "chain", "mean", "sd", "min", "max"),
    description = c(
      "Rolling sum (accumulation of flows)",
      "Chained accumulation of rates, prod(1 + r) - 1",
      "Rolling mean",
      "Rolling standard deviation",
      "Rolling minimum",
      "Rolling maximum"
    ),
    stringsAsFactors = FALSE
  )
}

#' Canonical vector of valid rolling statistic names
#'
#' @description Returns the statistic names supported by [augment_rolling()]
#' and [roll_series()] in their canonical order. Used by input validation.
#' @noRd
.valid_rolling_stats <- function() {
  return(.rolling_info()$stat)
}
