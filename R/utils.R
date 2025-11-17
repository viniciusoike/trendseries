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

#' Ensure window size is odd
#' @description For methods that require odd windows (median, gaussian),
#' auto-adjust even frequencies to the next odd number.
#' @noRd
.ensure_odd_window <- function(freq) {
  if (freq %% 2 == 0) {
    return(freq + 1)
  } else {
    return(freq)
  }
}

#' Get method category for parameter mapping
#' @noRd
.get_method_category <- function(method) {
  method_categories <- list(
    moving_average = c("ma", "wma", "zlema", "triangular", "stl"),
    smoothing = c(
      "hp", "loess", "spline", "ewma",
      "kernel", "kalman", "median", "gaussian"
    ),
    bandpass = c("bk", "cf"),
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
  align = NULL,
  frequency
) {
  unified_params <- list()

  # Process window parameter for moving average methods
  if (!is.null(window)) {
    window_methods <- c("ma", "wma", "triangular", "stl", "ewma", "median", "gaussian")
    for (method in methods[methods %in% window_methods]) {
      unified_params <- switch(
        method,
        "ma" = c(unified_params, list(ma_window = window)),
        "wma" = c(unified_params, list(wma_window = window)),
        "triangular" = c(unified_params, list(triangular_window = window)),
        "stl" = c(unified_params, list(stl_s_window = window)),
        "ewma" = c(unified_params, list(ewma_window = window)),
        "median" = c(unified_params, list(median_window = window)),
        "gaussian" = c(unified_params, list(gaussian_window = window)),
        unified_params
      )
    }
  }

  # Process align parameter for moving average methods that support alignment
  if (!is.null(align)) {
    align_methods <- c("ma", "wma", "triangular", "gaussian")
    for (method in methods[methods %in% align_methods]) {
      unified_params <- switch(
        method,
        "ma" = c(unified_params, list(ma_align = align)),
        "wma" = c(unified_params, list(wma_align = align)),
        "triangular" = c(unified_params, list(triangular_align = align)),
        "gaussian" = c(unified_params, list(gaussian_align = align)),
        unified_params
      )
    }
  }

  # Process smoothing parameter for smoothing methods
  if (!is.null(smoothing)) {
    smoothing_methods <- c("hp", "loess", "spline", "ewma", "kernel", "kalman")
    for (method in methods[methods %in% smoothing_methods]) {
      unified_params <- switch(
        method,
        "hp" = c(
          unified_params,
          list(hp_lambda = if (smoothing > 1) {
            # If smoothing > 1, use it directly as lambda
            smoothing
          } else {
            # If smoothing <= 1, interpret as fraction and scale by frequency-appropriate lambda
            # Using Ravn & Uhlig (2002) formula: λ = 1600 * (freq_new / 4)^4
            base_lambda <- switch(
              as.character(frequency),
              "1" = 100,           # Annual: 6.25 (100/1600 ≈ 0.0625)
              "2" = 400,           # Semi-annual: 6.25 * 4 = 25 (simplified)
              "4" = 1600,          # Quarterly: 1600 (standard)
              "12" = 14400,        # Monthly: 129600 (actual) but 14400 is convention
              "52" = 270400,       # Weekly: ~270000
              "365" = 6331600,     # Daily: very high smoothing
              # General formula: 1600 * (freq/4)^4
              1600 * (frequency / 4)^4
            )
            smoothing * base_lambda
          })
        ),
        "loess" = c(unified_params, list(loess_span = smoothing)),
        "spline" = c(unified_params, list(spline_spar = smoothing)),
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
    bandpass_methods <- c("bk", "cf")
    for (method in methods[methods %in% bandpass_methods]) {
      unified_params <- c(
        unified_params,
        list(
          bk_low = band[1], bk_high = band[2],
          cf_low = band[1], cf_high = band[2]
        )
      )
    }
  }

  return(unified_params)
}

#' Normalize STL parameter names
#' @description Convert dot notation (s.window, t.window) to underscore notation
#' (stl_s_window, stl_t_window) for internal consistency
#' @noRd
.normalize_stl_params <- function(params) {
  if (length(params) == 0) {
    return(params)
  }

  # Map dot notation to underscore notation
  param_map <- c(
    "s.window" = "stl_s_window",
    "t.window" = "stl_t_window",
    "robust" = "stl_robust"
  )

  normalized <- params
  param_names <- names(params)

  for (old_name in names(param_map)) {
    if (old_name %in% param_names) {
      # Rename the parameter
      new_name <- param_map[old_name]
      names(normalized)[names(normalized) == old_name] <- new_name
    }
  }

  return(normalized)
}

#' Process unified parameters into method-specific parameters
#' @noRd
.process_unified_params <- function(methods, window, smoothing, band, align, params, frequency, .quiet = FALSE) {
  # Start with method-specific params
  all_params <- params

  # Validate user-provided params before processing
  .validate_params(methods, params, .quiet)

  # Normalize STL parameters if STL is in methods
  if ("stl" %in% methods) {
    all_params <- .normalize_stl_params(all_params)
  }

  # Add unified parameter mappings (these should NOT override user-provided params)
  unified_mappings <- .map_unified_params(methods, window, smoothing, band, align, frequency)

  # Only add unified mappings if they don't conflict with user params
  for (param_name in names(unified_mappings)) {
    if (!param_name %in% names(all_params)) {
      all_params[[param_name]] <- unified_mappings[[param_name]]
    }
  }

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
      "hp" = params[names(params) %in% c("hp_onesided")],
      "ma" = params[names(params) %in% c("ma_align")],
      "wma" = params[names(params) %in% c("wma_weights", "wma_align")],
      "triangular" = params[names(params) %in% c("triangular_align")],
      "stl" = params[names(params) %in% c("stl_s_window", "stl_t_window", "stl_robust")],
      "poly" = params[names(params) %in% c("poly_degree", "poly_raw")],
      "spline" = params[names(params) %in% c("spline_cv")],
      "ucm" = params[names(params) %in% c("ucm_type")],
      "bn" = params[names(params) %in% c("bn_ar_order")],
      "hamilton" = params[names(params) %in% c("hamilton_h", "hamilton_p")],
      "kernel" = params[names(params) %in% c("kernel_type")],
      "kalman" = params[
        names(params) %in% c("kalman_measurement_noise", "kalman_process_noise")
      ],
      "median" = params[names(params) %in% c("median_endrule")],
      "gaussian" = params[names(params) %in% c("gaussian_sigma", "gaussian_align")],
      list()
    ))
  }

  return(method_params)
}

#' Validate and warn about unrecognized parameters
#' @noRd
.validate_params <- function(methods, params, .quiet = FALSE) {
  if (length(params) == 0 || .quiet) {
    return(invisible(NULL))
  }

  # Define all recognized parameters by method
  recognized_params <- list(
    hp = c("hp_lambda", "hp_onesided"),
    bk = c("bk_low", "bk_high"),
    cf = c("cf_low", "cf_high"),
    ma = c("ma_window", "ma_align"),
    stl = c("stl_s_window", "stl_t_window", "stl_robust", "s.window", "t.window", "robust"),
    loess = c("loess_span"),
    spline = c("spline_spar", "spline_cv"),
    poly = c("poly_degree", "poly_raw"),
    bn = c("bn_ar_order"),
    ucm = c("ucm_type"),
    hamilton = c("hamilton_h", "hamilton_p"),
    spencer = c(),  # No additional params
    ewma = c("ewma_alpha", "ewma_window"),
    wma = c("wma_window", "wma_weights", "wma_align"),
    triangular = c("triangular_window", "triangular_align"),
    kernel = c("kernel_bandwidth", "kernel_type"),
    kalman = c("kalman_smoothing", "kalman_measurement_noise", "kalman_process_noise"),
    median = c("median_window", "median_endrule"),
    gaussian = c("gaussian_window", "gaussian_sigma", "gaussian_align")
  )

  # Collect all recognized parameters for the selected methods
  all_recognized <- unique(unlist(recognized_params[methods]))

  # Find unrecognized parameters
  param_names <- names(params)
  unrecognized <- setdiff(param_names, all_recognized)

  if (length(unrecognized) > 0) {
    cli::cli_warn(
      "Unrecognized parameters in {.arg params}: {.val {unrecognized}}.
       These parameters will be ignored. Check ?extract_trends for valid parameter names."
    )
  }

  return(invisible(NULL))
}

#' Check for deprecated parameters and provide warnings
#' @noRd
.check_deprecated_params <- function(...) {
  dots <- list(...)
  deprecated_params <- c(
    "hp_lambda", "ma_window", "stl_s_window", "loess_span", "spline_spar",
    "poly_degree", "bk_low", "bk_high", "cf_low", "cf_high", "bn_ar_order",
    "hamilton_h", "hamilton_p", "ewma_alpha",
    "wma_window", "wma_weights", "zlema_window", "zlema_ratio",
    "triangular_window", "triangular_align",
    "kernel_bandwidth", "kernel_type",
    "kalman_measurement_noise", "kalman_process_noise"
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

#' List Available Datasets
#'
#' @description
#' Returns a tibble with metadata for all datasets included in the trendseries package.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{name}{Dataset name}
#'   \item{description}{Brief description of the dataset}
#'   \item{frequency}{Data frequency (D = daily, M = monthly, Q = quarterly)}
#'   \item{n_obs}{Number of observations}
#'   \item{first_date}{First observation date}
#'   \item{last_date}{Last observation date}
#'   \item{value_cols}{Main value column(s) in the dataset}
#'   \item{source}{Data source}
#' }
#'
#' @examples
#' # List all available datasets
#' list_datasets()
#'
#' # Filter for monthly data
#' list_datasets() |>
#'   dplyr::filter(frequency == "M")
#'
#' @export
list_datasets <- function() {
  # Define all datasets
  datasets <- c(
    "gdp_construction", "ibcbr", "vehicles", "electric", "oil_derivatives",
    "retail_households", "retail_autofuel", "coffee_arabica", "coffee_robusta"
  )

  # Create metadata for each dataset
  metadata_list <- lapply(datasets, function(ds_name) {
    # Load dataset
    ds <- get(ds_name, envir = asNamespace("trendseries"))

    # Get basic info
    n_obs <- nrow(ds)
    first_date <- min(ds$date, na.rm = TRUE)
    last_date <- max(ds$date, na.rm = TRUE)

    # Determine frequency
    if (n_obs > 1) {
      date_diff <- as.numeric(diff(ds$date[1:min(3, n_obs)]))
      avg_diff <- mean(date_diff, na.rm = TRUE)

      frequency <- dplyr::case_when(
        avg_diff <= 2 ~ "D",          # Daily
        avg_diff <= 10 ~ "W",         # Weekly
        avg_diff <= 35 ~ "M",         # Monthly
        avg_diff <= 100 ~ "Q",        # Quarterly
        TRUE ~ "Y"                    # Yearly or other
      )
    } else {
      frequency <- NA_character_
    }

    # Get value columns (exclude date and metadata columns)
    metadata_cols <- c("date", "name", "frequency", "source")
    value_cols <- setdiff(names(ds), metadata_cols)
    value_cols_str <- paste(value_cols, collapse = ", ")

    # Dataset-specific metadata
    metadata <- switch(
      ds_name,
      "gdp_construction" = list(
        description = "GDP Construction Index",
        source = "BCB-SGS 22087"
      ),
      "ibcbr" = list(
        description = "Central Bank Economic Activity Index",
        source = "BCB-SGS 24363"
      ),
      "vehicles" = list(
        description = "Vehicle Production",
        source = "BCB-SGS 1378"
      ),
      "electric" = list(
        description = "Electric Consumption Residential",
        source = "BCB-SGS 1403"
      ),
      "oil_derivatives" = list(
        description = "Oil Derivatives Production",
        source = "BCB-SGS 1391"
      ),
      "retail_households" = list(
        description = "UK Retail Sales - Household Goods Stores",
        source = "ONS"
      ),
      "retail_autofuel" = list(
        description = "UK Retail Sales - Automotive Fuel",
        source = "ONS"
      ),
      "coffee_arabica" = list(
        description = "CEPEA Arabica Coffee Prices",
        source = "CEPEA/ESALQ"
      ),
      "coffee_robusta" = list(
        description = "CEPEA Robusta Coffee Prices",
        source = "CEPEA/ESALQ"
      ),
      list(description = "", source = "")
    )

    # Return tibble row
    tibble::tibble(
      name = ds_name,
      description = metadata$description,
      frequency = frequency,
      n_obs = n_obs,
      first_date = first_date,
      last_date = last_date,
      value_cols = value_cols_str,
      source = metadata$source
    )
  })

  # Combine all metadata
  result <- dplyr::bind_rows(metadata_list)

  return(result)
}