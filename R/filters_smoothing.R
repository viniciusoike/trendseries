#' Statistical Smoothing Methods
#'
#' @description Internal functions for statistical smoothing and regression-based
#' trend extraction methods including loess, splines, polynomial fitting, STL
#' decomposition, median filtering, and Gaussian filtering.
#'
#' @name smoothing-filters
#' @keywords internal

#' Extract loess trend
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

#' Extract spline trend
#' @noRd
.extract_spline_trend <- function(ts_data, spar, cv, .quiet) {
  # Build informational message
  if (!.quiet) {
    spar_msg <- if (is.null(spar)) "automatic smoothing" else "spar = {spar}"
    cv_msg <- if (is.null(cv)) {
      "no CV"
    } else if (cv) {
      "leave-one-out CV"
    } else {
      "GCV"
    }
    cli::cli_inform("Computing spline trend with {spar_msg}, {cv_msg}")
  }

  # Create time index
  time_index <- as.numeric(stats::time(ts_data))
  values <- as.numeric(ts_data)

  # Fit smoothing spline
  if (is.null(spar) && is.null(cv)) {
    spline_fit <- stats::smooth.spline(time_index, values)
  } else if (is.null(spar) && !is.null(cv)) {
    spline_fit <- stats::smooth.spline(time_index, values, cv = cv)
  } else if (!is.null(spar) && is.null(cv)) {
    spline_fit <- stats::smooth.spline(time_index, values, spar = spar)
  } else {
    spline_fit <- stats::smooth.spline(time_index, values, spar = spar, cv = cv)
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

#' Extract polynomial trend
#' @noRd
.extract_poly_trend <- function(ts_data, degree, raw, .quiet) {
  # Validate degree and warn if too high
  if (degree > 3 && !.quiet) {
    cli::cli_warn(
      "Polynomial degree > 3 detected (degree = {degree}).
      High-degree polynomials are prone to overfitting and may produce unrealistic trends.
      Consider using degree <= 3 or alternative smoothing methods."
    )
  }

  if (!.quiet) {
    poly_type <- if (raw) "raw" else "orthogonal"
    cli::cli_inform(
      "Computing {poly_type} polynomial trend with degree = {degree}"
    )
  }

  # Create time index
  time_index <- as.numeric(stats::time(ts_data))
  values <- as.numeric(ts_data)

  # Fit polynomial
  poly_fit <- stats::lm(
    values ~ stats::poly(time_index, degree = degree, raw = raw)
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

#' Extract STL trend
#' @noRd
.extract_stl_trend <- function(ts_data, s_window, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing STL trend with s.window = {s_window}")
  }

  # Check if series has enough seasonality for STL
  freq <- stats::frequency(ts_data)
  if (freq == 1) {
    if (!.quiet) {
      cli::cli_warn(
        "STL not applicable for non-seasonal data. Using HP filter instead."
      )
    }
    return(.extract_hp_trend(ts_data, lambda = 1600, .quiet = TRUE))
  }

  stl_result <- stats::stl(ts_data, s.window = s_window)
  return(stl_result$time.series[, "trend"])
}

#' Extract median filter trend
#' @noRd
.extract_median_trend <- function(ts_data, window, endrule, .quiet) {
  # Validate window parameter
  n <- length(ts_data)
  if (window < 3) {
    cli::cli_abort("Median filter window must be at least 3, got {window}")
  }
  if (window > n) {
    cli::cli_abort(
      "Median filter window ({window}) cannot exceed series length ({n})"
    )
  }
  if (window %% 2 == 0) {
    cli::cli_abort(
      "Median filter window must be odd, got {window}"
    )
  }

  # Validate endrule parameter
  valid_endrules <- c("median", "keep", "constant")
  if (!endrule %in% valid_endrules) {
    cli::cli_abort(
      "endrule must be one of {.val {valid_endrules}}, got {.val {endrule}}"
    )
  }

  if (!.quiet) {
    cli::cli_inform(
      "Computing {window}-period median filter with endrule = {endrule}"
    )
  }

  return(.median_filter(ts_data, window, endrule))
}

#' Median Filter using stats::runmed
#' @noRd
.median_filter <- function(ts_data, window = 5, endrule = "median") {
  # Use stats::runmed for efficient median filtering with Turlach's algorithm
  median_result <- stats::runmed(as.numeric(ts_data), k = window, endrule = endrule)

  # Convert back to ts object
  trend_ts <- stats::ts(
    median_result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract Gaussian filter trend
#' @noRd
.extract_gaussian_trend <- function(ts_data, window, sigma, align, .quiet) {
  # Validate window parameter
  n <- length(ts_data)
  if (window < 3) {
    cli::cli_abort("Gaussian filter window must be at least 3, got {window}")
  }
  if (window > n) {
    cli::cli_abort(
      "Gaussian filter window ({window}) cannot exceed series length ({n})"
    )
  }
  if (window %% 2 == 0) {
    cli::cli_abort(
      "Gaussian filter window must be odd, got {window}"
    )
  }

  # Set default sigma if not provided (window/4 provides good coverage)
  if (is.null(sigma)) {
    sigma <- window / 4
  }

  # Validate sigma parameter
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    cli::cli_abort(
      "Gaussian filter sigma must be a positive numeric value, got {sigma}"
    )
  }

  # Validate align parameter
  if (!align %in% c("center", "right")) {
    cli::cli_abort(
      "Gaussian filter align must be 'center' or 'right', got {.val {align}}"
    )
  }

  if (!.quiet) {
    sigma_msg <- as.character(round(sigma, 2))
    cli::cli_inform(
      "Computing {window}-period Gaussian filter with sigma = {sigma_msg}, {align} alignment"
    )
  }

  return(.gaussian_filter(ts_data, window, sigma, align))
}

#' Gaussian Filter with normal density weights
#' @noRd
.gaussian_filter <- function(ts_data, window = 7, sigma = NULL, align = "center") {
  # Set default sigma if not provided
  if (is.null(sigma)) {
    sigma <- window / 4
  }

  # Create Gaussian weights
  half_window <- (window - 1) / 2
  x <- seq(-half_window, half_window, by = 1)
  weights <- stats::dnorm(x, mean = 0, sd = sigma)

  # Normalize weights to sum to 1
  weights <- weights / sum(weights)

  # Set sides parameter based on alignment
  sides <- if (align == "center") 2L else 1L

  # Use stats::filter for efficient convolution with Gaussian weights
  result <- stats::filter(
    as.numeric(ts_data),
    filter = weights,
    method = "convolution",
    sides = sides
  )

  # Convert back to ts object
  trend_ts <- stats::ts(
    as.numeric(result),
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )

  return(trend_ts)
}

