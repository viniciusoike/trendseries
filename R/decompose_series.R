#' Decompose time series into trend, seasonal, and remainder components
#'
#' @description
#' Pipe-friendly function that decomposes a time series into its trend, seasonal,
#' and remainder components, adding them as columns to the input data frame.
#' Supports STL decomposition and regression-based decomposition.
#'
#' @param data A `data.frame`, `tibble`, or `data.table` containing the time series data.
#' @param date_col Name of the date column. Defaults to `"date"`. Must be of class `Date`.
#' @param value_col Name of the value column. Defaults to `"value"`. Must be numeric.
#' @param group_cols Optional grouping variables for multiple time series.
#'   A character vector of column names. When provided, decomposition is applied
#'   independently to each group.
#' @param method Decomposition method. One of `"stl"` or `"regression"`.
#'   Default is `"stl"`.
#' @param trend For `method = "regression"` only: the polynomial form of the trend
#'   component. One of `"linear"` (degree 1), `"quadratic"` (degree 2), or
#'   `"cubic"` (degree 3). Ignored when `method = "stl"`. Default is `"linear"`.
#' @param frequency The frequency of the series. Supports 4 (quarterly) or 12
#'   (monthly). Will be auto-detected if not specified. Both methods require
#'   `frequency > 1`.
#' @param params Optional list of method-specific parameters for fine control.
#'   Sensible defaults are provided for all parameters; this argument is only
#'   needed for non-standard use cases.
#'
#'   For **STL** (`method = "stl"`):
#'   - `s.window` or `stl_s_window`: seasonal smoothing window. Either `"periodic"`
#'     (default, assumes constant seasonal pattern) or a positive odd integer
#'     (larger values allow more slowly evolving seasonality).
#'   - `t.window` or `stl_t_window`: trend smoothing window (odd integer, or `NULL`
#'     to let `stats::stl()` choose automatically — recommended default).
#'   - `robust` or `stl_robust`: logical. If `TRUE`, uses robust fitting to reduce
#'     the influence of outliers. Default `FALSE`.
#'
#'   For **regression** (`method = "regression"`):
#'   - `poly_raw`: logical. If `FALSE` (default), uses orthogonal polynomials
#'     (numerically stable, recommended). If `TRUE`, uses raw polynomials
#'     (more interpretable coefficients, less stable for degree >= 2).
#'
#' @param .quiet If `TRUE`, suppress informational messages.
#'
#' @return A tibble with the original columns plus three new columns:
#'   - `trend_{method}`: the estimated trend component.
#'   - `seasonal_{method}`: the estimated seasonal component.
#'   - `remainder_{method}`: what remains after removing trend and seasonal.
#'
#'   The identity `value = trend + seasonal + remainder` holds exactly for
#'   both methods.
#'
#' @details
#' Both methods require seasonal data (`frequency > 1`). For non-seasonal
#' (annual) series, use [augment_trends()] to extract a trend component only.
#'
#' ## STL Decomposition
#'
#' Uses `stats::stl()` (Seasonal-Trend decomposition via Loess). The seasonal
#' component is estimated with a loess smoother, the trend with an adaptive
#' moving average, and the remainder is the residual. Default settings
#' (`s.window = "periodic"`, `robust = FALSE`) are appropriate for most
#' economic series with stable seasonal patterns.
#'
#' ## Regression Decomposition
#'
#' Fits a joint OLS model:
#' \deqn{y_t = f(t) + s(t) + \epsilon_t}
#' where \eqn{f(t)} is a polynomial in time and \eqn{s(t)} is captured by
#' period dummy variables (month or quarter indicators). The components are
#' isolated via the fitted model matrix:
#' - **Trend**: intercept + polynomial terms (captures the long-run level and direction).
#' - **Seasonal**: period dummy variable terms (captures deviation from the baseline period).
#' - **Remainder**: residuals from the full model.
#'
#' By default, orthogonal polynomials (`poly_raw = FALSE`) are used for numerical
#' stability. For `trend = "cubic"`, this is especially recommended.
#'
#' ## Multiplicative Seasonality
#'
#' Both methods are additive by design. If the seasonal amplitude grows with the
#' level of the series (multiplicative pattern), log-transform the values before
#' calling `decompose_series()`:
#'
#' ```r
#' data |>
#'   dplyr::mutate(log_value = log(value)) |>
#'   decompose_series(value_col = "log_value")
#' ```
#'
#' The components will be on the log scale; exponentiate them to recover
#' multiplicative components where `trend * seasonal * remainder = value`.
#'
#' @examples
#' # STL decomposition (default settings work well for most economic series)
#' gdp_construction |>
#'   decompose_series(value_col = "index")
#'
#' # STL with robust fitting (useful when the series has outliers)
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     params = list(robust = TRUE)
#'   )
#'
#' # STL with evolving seasonality (s.window controls how fast it can change)
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     params = list(s.window = 13)
#'   )
#'
#' # Regression with linear trend + quarterly dummies (default)
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     method = "regression"
#'   )
#'
#' # Regression with quadratic trend (captures accelerating/decelerating growth)
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     method = "regression",
#'     trend = "quadratic"
#'   )
#'
#' # Regression with cubic trend
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     method = "regression",
#'     trend = "cubic"
#'   )
#'
#' # Grouped decomposition: one decomposition per electricity sector
#' electricity |>
#'   decompose_series(
#'     group_cols = "name_series"
#'   )
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom tibble as_tibble
#' @importFrom tsbox ts_df
#' @importFrom stats stl lm poly model.matrix coef cycle ts start time frequency
#'
#' @export
decompose_series <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  method = "stl",
  trend = "linear",
  frequency = NULL,
  params = list(),
  .quiet = FALSE
) {
  # --- Input validation ---
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame, tibble, or data.table")
  }

  if (!date_col %in% names(data)) {
    cli::cli_abort("Column {.val {date_col}} not found in data")
  }

  if (!value_col %in% names(data)) {
    cli::cli_abort("Column {.val {value_col}} not found in data")
  }

  if (!inherits(data[[date_col]], "Date")) {
    cli::cli_abort("Column {.val {date_col}} must be of class Date")
  }

  if (!is.numeric(data[[value_col]])) {
    cli::cli_abort("Column {.val {value_col}} must be numeric")
  }

  valid_methods <- c("stl", "regression")
  if (length(method) != 1 || !method %in% valid_methods) {
    cli::cli_abort(
      "Invalid method {.val {method}}. Valid options: {.val {valid_methods}}"
    )
  }

  valid_trends <- c("linear", "quadratic", "cubic")
  if (length(trend) != 1 || !trend %in% valid_trends) {
    cli::cli_abort(
      "Invalid trend {.val {trend}}. Valid options: {.val {valid_trends}}"
    )
  }

  if (!is.null(group_cols)) {
    if (!is.character(group_cols)) {
      cli::cli_abort("{.arg group_cols} must be a character vector")
    }
    missing_group_cols <- setdiff(group_cols, names(data))
    if (length(missing_group_cols) > 0) {
      cli::cli_abort(
        "Group columns not found in data: {.val {missing_group_cols}}"
      )
    }
  }

  if (!is.list(params)) {
    cli::cli_abort("{.arg params} must be a list")
  }

  # Convert to tibble for consistent handling
  data <- tibble::as_tibble(data)

  # Dispatch
  if (is.null(group_cols)) {
    result <- .decompose_series_single(
      data = data,
      date_col = date_col,
      value_col = value_col,
      method = method,
      trend = trend,
      frequency = frequency,
      params = params,
      .quiet = .quiet
    )
  } else {
    result <- .decompose_series_grouped(
      data = data,
      date_col = date_col,
      value_col = value_col,
      group_cols = group_cols,
      method = method,
      trend = trend,
      frequency = frequency,
      params = params,
      .quiet = .quiet
    )
  }

  return(result)
}

# ---------------------------------------------------------------------------
# Internal: single-series handler
# ---------------------------------------------------------------------------

#' @noRd
.decompose_series_single <- function(
  data,
  date_col,
  value_col,
  method,
  trend,
  frequency,
  params,
  .quiet
) {
  # Auto-detect frequency
  if (is.null(frequency)) {
    frequency <- .detect_frequency(data[[date_col]], .quiet = .quiet)
  }

  # Both methods require seasonal data (frequency > 1)
  if (frequency == 1) {
    cli::cli_abort(c(
      "{.val {method}} decomposition requires seasonal data ({.arg frequency} > 1).",
      "i" = "Annual series have no seasonal component to decompose.",
      "i" = "Use {.fn augment_trends} to extract a trend component from non-seasonal series."
    ))
  }

  # Warn for short series
  min_obs <- 3L * frequency
  n_obs <- nrow(data)
  if (n_obs < min_obs && !.quiet) {
    cli::cli_warn(
      "Series has {n_obs} observations. Minimum {min_obs} recommended for reliable decomposition."
    )
  }

  # Convert to ts
  ts_data <- .df_to_ts_internal(data, date_col, value_col, frequency)

  # Decompose
  components <- switch(
    method,
    "stl" = .decompose_stl(ts_data, params, .quiet),
    "regression" = .decompose_regression(ts_data, trend, params, .quiet)
  )

  # Convert components to a date-keyed data frame, then merge back on the date
  # column. This is robust to NA-induced length mismatches (rows with missing
  # values are dropped by .df_to_ts_internal before fitting) and to date
  # convention differences (e.g. end-of-month vs first-of-month) because
  # .safe_merge() normalises both sides with lubridate::floor_date().
  components_df <- .decompose_components_to_df(components, method, date_col)
  result <- .safe_merge(data, components_df, date_col, frequency)

  return(result)
}

# ---------------------------------------------------------------------------
# Internal: grouped handler
# ---------------------------------------------------------------------------

#' @noRd
.decompose_series_grouped <- function(
  data,
  date_col,
  value_col,
  group_cols,
  method,
  trend,
  frequency,
  params,
  .quiet
) {
  data_split <- split(data, data[group_cols])
  group_names <- names(data_split)

  # Detect frequency once from the first group
  if (is.null(frequency)) {
    frequency <- .detect_frequency(data_split[[1]][[date_col]], .quiet = .quiet)
  }

  if (!.quiet) {
    cli::cli_inform(c(
      "Decomposing {length(group_names)} group(s) using {.val {method}} method:",
      "i" = "Groups: {.val {group_names}}"
    ))
  }

  results <- lapply(data_split, function(group_data) {
    .decompose_series_single(
      data = group_data,
      date_col = date_col,
      value_col = value_col,
      method = method,
      trend = trend,
      frequency = frequency,
      params = params,
      .quiet = TRUE
    )
  })

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  return(result)
}

# ---------------------------------------------------------------------------
# Internal: convert components list to a date-keyed data frame
# ---------------------------------------------------------------------------

#' Convert a named list of ts components to a single data frame
#'
#' Uses tsbox::ts_df() to recover dates from each ts object, then merges the
#' three component columns on the date column. This is the same approach used
#' by .trends_to_df() + .safe_merge() in augment_trends() and is robust to:
#' - NA-induced length differences (ts may be shorter than nrow(data))
#' - Date convention differences (handled by .safe_merge via floor_date)
#'
#' @noRd
.decompose_components_to_df <- function(components, method, date_col) {
  .ts_to_col <- function(ts_obj, col_name) {
    df <- tsbox::ts_df(ts_obj)
    names(df) <- c(date_col, col_name)
    return(df)
  }

  trend_df <- .ts_to_col(components$trend, paste0("trend_", method))
  seasonal_df <- .ts_to_col(components$seasonal, paste0("seasonal_", method))
  remainder_df <- .ts_to_col(components$remainder, paste0("remainder_", method))

  result <- merge(trend_df, seasonal_df, by = date_col, all = TRUE)
  result <- merge(result, remainder_df, by = date_col, all = TRUE)

  return(tibble::as_tibble(result))
}

# ---------------------------------------------------------------------------
# Internal: STL decomposition
# ---------------------------------------------------------------------------

#' @noRd
.decompose_stl <- function(ts_data, params, .quiet) {
  # Accept both dot notation (s.window) and underscore notation (stl_s_window).
  # Defaults: s.window = "periodic" (stable seasonal pattern), robust = FALSE.
  s_window <- params[["s.window"]] %||% params[["stl_s_window"]] %||% "periodic"
  t_window <- params[["t.window"]] %||% params[["stl_t_window"]] %||% NULL
  robust <- params[["robust"]] %||% params[["stl_robust"]] %||% FALSE

  if (!.quiet) {
    msg <- paste0("s.window = ", deparse(s_window))
    if (!is.null(t_window)) {
      msg <- paste0(msg, ", t.window = ", t_window)
    }
    if (isTRUE(robust)) {
      msg <- paste0(msg, ", robust = TRUE")
    }
    cli::cli_inform("Computing STL decomposition with {msg}")
  }

  stl_args <- list(x = ts_data, s.window = s_window, robust = robust)
  if (!is.null(t_window)) {
    stl_args$t.window <- t_window
  }

  stl_result <- do.call(stats::stl, stl_args)
  ts_mat <- stl_result$time.series # columns: seasonal, trend, remainder

  return(list(
    trend = ts_mat[, "trend"],
    seasonal = ts_mat[, "seasonal"],
    remainder = ts_mat[, "remainder"]
  ))
}

# ---------------------------------------------------------------------------
# Internal: regression-based decomposition
# ---------------------------------------------------------------------------

#' @noRd
.decompose_regression <- function(ts_data, trend, params, .quiet) {
  # Map trend name to polynomial degree
  degree <- switch(
    trend,
    "linear" = 1L,
    "quadratic" = 2L,
    "cubic" = 3L
  )

  # Default: orthogonal polynomials (numerically stable, especially for degree >= 2)
  poly_raw <- params[["poly_raw"]] %||% FALSE

  freq <- stats::frequency(ts_data)
  values <- as.numeric(ts_data)
  t_idx <- as.numeric(stats::time(ts_data))

  # Period indicator: 1..freq (month for monthly, quarter for quarterly)
  period_factor <- factor(as.integer(stats::cycle(ts_data)))

  if (!.quiet) {
    poly_type <- if (poly_raw) "raw" else "orthogonal"
    cli::cli_inform(
      "Computing regression decomposition: {trend} trend ({poly_type} polynomial, degree = {degree}) + {freq}-period dummies"
    )
  }

  # Joint model: polynomial trend + period dummy variables.
  # Using stats::poly() for the trend avoids multicollinearity in higher degrees.
  fit <- stats::lm(
    values ~ stats::poly(t_idx, degree = degree, raw = poly_raw) + period_factor
  )

  # Isolate trend and seasonal contributions via the model matrix.
  # Seasonal columns: all period_factor dummies (start with "period_factor").
  # Trend columns: everything else (intercept + polynomial terms).
  mm <- stats::model.matrix(fit)
  coefs <- stats::coef(fit)

  # Drop any aliased (NA) coefficients that can arise with degenerate data
  valid <- !is.na(coefs)
  mm_valid <- mm[, valid, drop = FALSE]
  coefs_valid <- coefs[valid]
  col_names <- colnames(mm_valid)

  is_seasonal <- grepl("^period_factor", col_names)
  is_trend <- !is_seasonal

  trend_vals <- as.vector(
    mm_valid[, is_trend, drop = FALSE] %*% coefs_valid[is_trend]
  )
  seasonal_vals <- as.vector(
    mm_valid[, is_seasonal, drop = FALSE] %*% coefs_valid[is_seasonal]
  )

  # Remainder = residuals of the full model.
  # Computed as y - trend - seasonal to guarantee the exact identity
  # trend + seasonal + remainder = value.
  remainder_vals <- values - trend_vals - seasonal_vals

  .make_ts <- function(x) {
    stats::ts(x, start = stats::start(ts_data), frequency = freq)
  }

  return(list(
    trend = .make_ts(trend_vals),
    seasonal = .make_ts(seasonal_vals),
    remainder = .make_ts(remainder_vals)
  ))
}
