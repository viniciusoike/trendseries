#' Utility Functions
#'
#' @description Core utility functions for the trendseries package including
#' parameter processing, method categorization, and helper operators.
#'
#' @name utils
#' @keywords internal
NULL

#' Null coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Get method category for parameter mapping
#' @noRd
.get_method_category <- function(method) {
  method_categories <- list(
    moving_average = c("ma", "alma", "dema", "hma", "stl"),
    smoothing = c(
      "hp", "loess", "spline", "exp_simple", "exp_double", "ewma",
      "sg", "kernel", "kalman"
    ),
    bandpass = c("bk", "cf", "butter"),
    special = c("stl", "poly", "bn", "hamilton", "ucm")
  )

  for (category in names(method_categories)) {
    if (method %in% method_categories[[category]]) {
      return(category)
    }
  }

  return("other")
}

#' Map unified parameters to method-specific parameters
#' @noRd
.map_unified_params <- function(
  methods,
  window = NULL,
  smoothing = NULL,
  band = NULL,
  frequency
) {
  unified_params <- list()

  # Process window parameter for moving average methods
  if (!is.null(window)) {
    window_methods <- c("ma", "alma", "dema", "hma", "stl", "sg", "ewma")
    for (method in methods[methods %in% window_methods]) {
      unified_params <- switch(
        method,
        "ma" = c(unified_params, list(ma_window = window)),
        "alma" = c(unified_params, list(alma_window = window)),
        "dema" = c(unified_params, list(dema_period = window)),
        "hma" = c(unified_params, list(hma_period = window)),
        "stl" = c(unified_params, list(stl_s_window = window)),
        "sg" = c(unified_params, list(sg_window = window)),
        "ewma" = c(unified_params, list(ewma_window = window)),
        unified_params
      )
    }
  }

  # Process smoothing parameter for smoothing methods
  if (!is.null(smoothing)) {
    smoothing_methods <- c("hp", "loess", "spline", "exp_simple", "exp_double", "ewma", "kernel", "kalman")
    for (method in methods[methods %in% smoothing_methods]) {
      unified_params <- switch(
        method,
        "hp" = c(
          unified_params,
          list(hp_lambda = if (smoothing > 1) smoothing else if (frequency == 4) smoothing * 1600 else smoothing * 14400)
        ),
        "loess" = c(unified_params, list(loess_span = smoothing)),
        "spline" = c(unified_params, list(spline_spar = smoothing)),
        "exp_simple" = c(unified_params, list(exp_alpha = smoothing)),
        "exp_double" = c(unified_params, list(exp_alpha = smoothing)),
        "ewma" = c(unified_params, list(ewma_alpha = smoothing)),
        "kernel" = c(
          unified_params,
          list(
            kernel_bandwidth = smoothing  # Pass smoothing directly, will be multiplied by auto bandwidth
          )
        ),
        "kalman" = c(unified_params, list(kalman_smoothing = smoothing)),
        unified_params
      )
    }
  }

  # Process band parameter for bandpass methods
  if (!is.null(band) && length(band) >= 2) {
    bandpass_methods <- c("bk", "cf", "butter")
    for (method in methods[methods %in% bandpass_methods]) {
      if (method %in% c("bk", "cf")) {
        unified_params <- c(
          unified_params,
          list(
            bk_low = band[1], bk_high = band[2],
            cf_low = band[1], cf_high = band[2]
          )
        )
      } else if (method == "butter") {
        unified_params <- c(
          unified_params,
          list(butter_cutoff = band[1], butter_order = band[2])
        )
      }
    }
  }

  return(unified_params)
}

#' Process unified parameters into method-specific parameters
#' @noRd
.process_unified_params <- function(methods, window, smoothing, band, params, frequency) {
  # Start with method-specific params
  all_params <- params

  # Add unified parameter mappings
  unified_mappings <- .map_unified_params(methods, window, smoothing, band, frequency)
  all_params <- c(all_params, unified_mappings)

  # Extract method-specific parameters
  method_specific <- .extract_method_params(methods, all_params)
  all_params <- c(all_params, method_specific)

  return(all_params)
}

#' Extract method-specific parameters from params list
#' @noRd
.extract_method_params <- function(methods, params) {
  method_params <- list()

  for (method in methods) {
    method_params <- c(method_params, switch(
      method,
      "alma" = params[names(params) %in% c("alma_offset", "alma_sigma")],
      "exp_double" = params[names(params) %in% c("exp_beta")],
      "poly" = params[names(params) %in% c("poly_degree")],
      "bn" = params[names(params) %in% c("bn_ar_order")],
      "hamilton" = params[names(params) %in% c("hamilton_h", "hamilton_p")],
      "sg" = params[names(params) %in% c("sg_poly_order")],
      "kernel" = params[names(params) %in% c("kernel_type")],
      "butter" = params[names(params) %in% c("butter_type")],
      "kalman" = params[
        names(params) %in% c("kalman_measurement_noise", "kalman_process_noise")
      ],
      list()
    ))
  }

  return(method_params)
}

#' Check for deprecated parameters and provide warnings
#' @noRd
.check_deprecated_params <- function(...) {
  dots <- list(...)
  deprecated_params <- c(
    "hp_lambda", "ma_window", "stl_s_window", "loess_span", "spline_spar",
    "poly_degree", "bk_low", "bk_high", "cf_low", "cf_high", "bn_ar_order",
    "hamilton_h", "hamilton_p", "exp_alpha", "exp_beta", "ewma_alpha",
    "alma_window", "alma_offset", "alma_sigma", "dema_period", "hma_period",
    "sg_window", "sg_poly_order", "kernel_bandwidth", "kernel_type",
    "butter_cutoff", "butter_order", "kalman_measurement_noise",
    "kalman_process_noise"
  )

  found_deprecated <- intersect(names(dots), deprecated_params)

  if (length(found_deprecated) > 0) {
    cli::cli_warn(
      "Deprecated parameters found: {.val {found_deprecated}}.
       Use unified parameters (window, smoothing, band) or pass via params list.
       See ?extract_trends for details."
    )

    # Convert deprecated params to new format suggestions
    suggestions <- character(0)
    if ("hp_lambda" %in% found_deprecated) suggestions <- c(suggestions, "Use 'smoothing' parameter")
    if (any(c("ma_window", "stl_s_window") %in% found_deprecated)) suggestions <- c(suggestions, "Use 'window' parameter")
    if (any(c("bk_low", "bk_high", "cf_low", "cf_high") %in% found_deprecated)) suggestions <- c(suggestions, "Use 'band = c(low, high)' parameter")

    if (length(suggestions) > 0) {
      cli::cli_inform("Suggestions: {suggestions}")
    }
  }

  return(invisible(NULL))
}