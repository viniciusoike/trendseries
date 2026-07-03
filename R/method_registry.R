#' Method registry --------------------------------------------------------

#' Canonical registry of trend extraction methods
#'
#' @description Single source of truth for the methods supported by
#' [augment_trends()] and [extract_trends()]. The validation routines (via
#' `.valid_methods()`) read from this table, so adding a method here propagates
#' everywhere. The catalogue is also surfaced to users in the
#' *Trend Extraction Methods* vignette.
#' @return A data frame with columns `method`, `category`, and `description`.
#' @noRd
.method_info <- function() {
  data.frame(
    method = c(
      "hp", "bk", "cf", "ma", "stl", "loess", "spline", "poly",
      "bn", "ucm", "hamilton", "spencer", "ewma", "wma", "triangular",
      "kernel", "kalman", "median", "gaussian", "henderson"
    ),
    category = c(
      "econometric", "bandpass", "bandpass", "moving_average", "smoothing",
      "smoothing", "smoothing", "smoothing", "econometric", "econometric",
      "econometric", "moving_average", "moving_average", "moving_average",
      "moving_average", "smoothing", "smoothing", "moving_average",
      "moving_average", "moving_average"
    ),
    description = c(
      "Hodrick-Prescott filter",
      "Baxter-King bandpass filter",
      "Christiano-Fitzgerald bandpass filter",
      "Simple moving average",
      "Seasonal-trend decomposition via Loess",
      "Local polynomial regression (loess)",
      "Smoothing splines",
      "Polynomial trend",
      "Beveridge-Nelson decomposition",
      "Unobserved components model",
      "Hamilton regression filter",
      "Spencer's 15-term moving average",
      "Exponentially weighted moving average",
      "Weighted moving average",
      "Triangular moving average",
      "Kernel smoother",
      "Kalman filter/smoother",
      "Median filter",
      "Gaussian-weighted moving average",
      "Henderson moving average"
    ),
    stringsAsFactors = FALSE
  )
}

#' Canonical vector of valid method names
#'
#' @description Returns the method names supported by [augment_trends()] and
#' [extract_trends()] in their canonical order. Used by input validation.
#' @noRd
.valid_methods <- function() {
  .method_info()$method
}

#' Canonical vector of decomposition method names
#'
#' @description Returns the method names supported by [decompose_series()] in
#' their canonical order. Used by input validation.
#' @noRd
.decompose_methods <- function() {
  return(c("stl", "regression", "classic", "bsm", "seats"))
}
