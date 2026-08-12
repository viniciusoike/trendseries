#' Decompose time series into trend, seasonal, and remainder components
#'
#' @description
#' Pipe-friendly function that decomposes a time series into its trend, seasonal,
#' and remainder components, adding them as columns to the input data frame.
#'
#' @param data A `data.frame`, `tibble`, or `data.table` containing the time series data.
#' @param date_col Name of the date column. Defaults to `"date"`. Must be of class `Date`.
#' @param value_col Name of the value column. Defaults to `"value"`. Must be numeric.
#' @param group_cols Optional grouping variables for multiple time series.
#'   A character vector of column names. When provided, decomposition is applied
#'   independently to each group.
#' @param methods Decomposition method(s). One or more of `"stl"`, `"regression"`,
#'   `"classic"`, `"bsm"`, or `"seats"`. Default is `"stl"`. When several methods
#'   are supplied (e.g. `c("stl", "classic")`), each one contributes its own
#'   `trend_*`, `seasonal_*`, and `remainder_*` columns so decompositions can be
#'   compared side by side.
#'   - `"stl"`: Seasonal-Trend decomposition via Loess (`stats::stl()`).
#'   - `"regression"`: joint OLS trend + seasonal-dummy model.
#'   - `"classic"`: classical decomposition via moving averages
#'     (`stats::decompose()`).
#'   - `"bsm"`: Basic Structural (state-space) Model via the Kalman smoother
#'     (`stats::StructTS()`).
#'   - `"seats"`: X-13ARIMA-SEATS decomposition (requires the
#'     **`seasonal`** package; see Details).
#' @param trend For `methods = "regression"` only: the polynomial form of the trend
#'   component. One of `"linear"`, `"quadratic"`, or
#'   `"cubic"`. Ignored by the other methods. Default is `"linear"`.
#' @param transform Transformation applied to the series before decomposition.
#'   One of `"none"` (default, additive decomposition) or `"log"`. With
#'   `"log"`, the series is log-transformed, decomposed additively, and the
#'   components are exponentiated back, yielding a *multiplicative* decomposition.
#' @param frequency The frequency of the series. Supports 4 (quarterly) or 12
#'   (monthly). Will be auto-detected if not specified. All methods require
#'   `frequency > 1`.
#' @param seasadj If `TRUE`, also add a `seasadj_{method}` column holding the
#'   seasonally adjusted series (the series with the seasonal component removed:
#'   `trend + remainder` for additive decompositions, `trend * remainder` for
#'   multiplicative ones). Default `FALSE`.
#' @param params Optional list of method-specific parameters for fine control.
#'   Every parameter has a default, so this argument is only needed for
#'   non-standard use cases.
#'
#'   For **STL** (`methods = "stl"`):
#'   - `s.window` or `stl_s_window`: seasonal smoothing window. Either `"periodic"`
#'     (default, assumes constant seasonal pattern) or a positive odd integer
#'     (larger values allow more slowly evolving seasonality).
#'   - `t.window` or `stl_t_window`: trend smoothing window (odd integer, or `NULL`
#'     to let `stats::stl()` choose automatically — recommended default).
#'   - `robust` or `stl_robust`: logical. If `TRUE`, uses robust fitting to reduce
#'     the influence of outliers. Default `FALSE`.
#'
#'   For **regression** (`methods = "regression"`):
#'   - `poly_raw`: logical. If `FALSE` (default), uses orthogonal polynomials
#'     (numerically stable, recommended). If `TRUE`, uses raw polynomials
#'     (more interpretable coefficients, less stable for degree >= 2).
#'
#'   **classic**, **bsm**, and **seats** take no `params`. For multiplicative
#'   seasonality with any method, use `transform = "log"`.
#'
#' @param .quiet If `TRUE`, suppress informational messages.
#'
#' @return A tibble with the original columns plus, for each requested method,
#'   three new columns (and a fourth when `seasadj = TRUE`):
#'   - `trend_{method}`: the estimated trend component.
#'   - `seasonal_{method}`: the estimated seasonal component.
#'   - `remainder_{method}`: what remains after removing trend and seasonal.
#'   - `seasadj_{method}`: the seasonally adjusted series (only if `seasadj = TRUE`).
#'
#'   With `transform = "none"` the components should add back up to the series
#'   (`value = trend + seasonal + remainder`); with `transform = "log"` they
#'   should multiply back to it (`value = trend * seasonal * remainder`).
#'   For `"classic"` the trend (and hence remainder) is `NA` for the first and
#'   last `frequency / 2` observations (the centred moving average has no
#'   boundary support).
#'
#'   Output rows are ordered by date within each group; the original row order
#'   is not preserved.
#'
#' @details
#' All methods require seasonal data (`frequency > 1`). For non-seasonal
#' (annual) series, use [augment_trends()] to extract a trend component only.
#'
#' ## STL Decomposition
#'
#' Uses `stats::stl()` (Seasonal-Trend decomposition via Loess). The seasonal
#' component is estimated with a loess smoother, the trend with an adaptive
#' moving average, and the remainder is the residual. The defaults
#' (`s.window = "periodic"`, `robust = FALSE`) assume a stable seasonal pattern.
#'
#' ## Regression Decomposition
#'
#' Fits a joint OLS model:
#' \deqn{y_t = f(t) + s(t) + \epsilon_t}
#' where \eqn{f(t)} is a polynomial in time and \eqn{s(t)} is captured by
#' period dummy variables (month or quarter indicators). The components are
#' isolated via `stats::predict(type = "terms")`:
#' - **Trend**: constant + polynomial terms (captures the long-run level and direction).
#' - **Seasonal**: period dummy terms, centred to mean zero over the sample.
#' - **Remainder**: residuals from the full model.
#'
#' By default, orthogonal polynomials (`poly_raw = FALSE`) are used for numerical
#' stability, which matters most for `trend = "cubic"`.
#'
#' ## Classical Decomposition
#'
#' Uses `stats::decompose()`. The trend is a centred moving average of order
#' equal to the frequency; the seasonal component is the average detrended value
#' for each period; the remainder is the residual. Simple and fast, but the
#' other methods handle evolving seasonality and the endpoints better.
#'
#' ## Basic Structural Model (BSM)
#'
#' Uses `stats::StructTS(type = "BSM")`, a state-space model with stochastic
#' level, slope, and seasonal components estimated by maximum likelihood and
#' extracted with the Kalman smoother (`stats::tsSmooth()`). Unlike the
#' moving-average methods it produces trend and seasonal estimates for every
#' observation, including the endpoints, and lets both components evolve over
#' time. Fitting relies on numerical optimisation and can occasionally fail to
#' converge on short or irregular series.
#'
#' ## X-13ARIMA-SEATS (SEATS)
#'
#' Uses the **`seasonal`** package, which wraps the U.S. Census Bureau's
#' X-13ARIMA-SEATS program. `seas()` is run with its automatic defaults (model
#' selection, log/level transformation, outlier detection, and calendar
#' adjustment), and the SEATS trend-cycle (`s12`) and seasonally adjusted series
#' (`s11`) are mapped to an additive trend/seasonal/remainder, whichever
#' transformation X-13 picked internally. Because X-13 picks that
#' transformation itself, `seats` is best used with the default
#' `transform = "none"`; an outer log transform is redundant.
#'
#' ## Multiplicative Seasonality
#'
#' When the seasonal amplitude grows with the level of the series (a
#' multiplicative pattern, common in economic data), set `transform = "log"`.
#' The series is log-transformed, decomposed additively, and the components are
#' exponentiated back. Every method takes this same path, which requires
#' strictly positive values.
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
#' # Regression with cubic trend
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     methods = "regression",
#'     trend = "cubic"
#'   )
#'
#' # Classical decomposition via moving averages (boundary trend is NA)
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     methods = "classic"
#'   )
#'
#' # Basic Structural Model (state-space, components for every observation)
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     methods = "bsm"
#'   )
#'
#' # X-13ARIMA-SEATS (requires the 'seasonal' package)
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   gdp_construction |>
#'     decompose_series(
#'       value_col = "index",
#'       methods = "seats"
#'     )
#' }
#'
#' # Multiplicative decomposition via log transform (works for any method)
#' oil_derivatives |>
#'   decompose_series(
#'     value_col = "production",
#'     transform = "log"
#'   )
#'
#' # Several methods at once for side-by-side comparison
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     methods   = c("stl", "classic")
#'   )
#'
#' # Also return the seasonally adjusted series
#' gdp_construction |>
#'   decompose_series(
#'     value_col = "index",
#'     seasadj   = TRUE
#'   )
#'
#' # Grouped decomposition: one decomposition per electricity sector
#' electricity |>
#'   decompose_series(
#'     group_cols = "name_series"
#'   )
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom rlang caller_env `!!` `:=`
#' @importFrom tibble as_tibble tibble
#' @importFrom tsbox ts_df
#' @importFrom stats stl lm poly predict cycle ts start time frequency
#'   decompose StructTS tsSmooth
#'
#' @export
decompose_series <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  methods = "stl",
  trend = "linear",
  transform = "none",
  frequency = NULL,
  seasadj = FALSE,
  params = list(),
  .quiet = FALSE
) {
  # --- Input validation ---
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame, tibble, or data.table")
  }

  .check_non_empty(data)

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

  valid_methods <- .decompose_methods()
  if (
    !is.character(methods) ||
      length(methods) < 1 ||
      !all(methods %in% valid_methods)
  ) {
    bad <- setdiff(methods, valid_methods)
    cli::cli_abort(
      "Invalid methods: {.val {bad}}. Valid options: {.val {valid_methods}}"
    )
  }
  # Drop duplicates while preserving order (avoids colliding output columns)
  methods <- unique(methods)

  valid_transforms <- c("none", "log")
  if (length(transform) != 1 || !transform %in% valid_transforms) {
    cli::cli_abort(
      "Invalid transform {.val {transform}}. Valid options: {.val {valid_transforms}}"
    )
  }

  if (!is.logical(seasadj) || length(seasadj) != 1 || is.na(seasadj)) {
    cli::cli_abort(
      "{.arg seasadj} must be a single {.code TRUE} or {.code FALSE}"
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

  if (!is.null(frequency)) {
    if (!is.numeric(frequency) || length(frequency) != 1 || frequency < 1) {
      cli::cli_abort("{.arg frequency} must be a single positive number")
    }
  }

  if (!("regression" %in% methods) && trend != "linear" && !.quiet) {
    cli::cli_warn(
      "{.arg trend} is ignored unless {.arg methods} includes {.val regression}"
    )
  }

  # Validate params keys
  .validate_params_keys(params, methods, .quiet)

  # Convert to tibble for consistent handling
  data <- tibble::as_tibble(data)

  # Dispatch
  call <- rlang::caller_env()

  if (is.null(group_cols)) {
    result <- .decompose_series_single(
      data = data,
      date_col = date_col,
      value_col = value_col,
      methods = methods,
      trend = trend,
      transform = transform,
      frequency = frequency,
      seasadj = seasadj,
      params = params,
      .quiet = .quiet,
      call = call
    )
  } else {
    result <- .decompose_series_grouped(
      data = data,
      date_col = date_col,
      value_col = value_col,
      group_cols = group_cols,
      methods = methods,
      trend = trend,
      transform = transform,
      frequency = frequency,
      seasadj = seasadj,
      params = params,
      .quiet = .quiet,
      call = call
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
  methods,
  trend,
  transform,
  frequency,
  seasadj,
  params,
  .quiet,
  call = rlang::caller_env()
) {
  # Auto-detect frequency
  if (is.null(frequency)) {
    frequency <- .detect_frequency(data[[date_col]], .quiet = .quiet)
  }

  # All methods require seasonal data (frequency > 1)
  if (frequency == 1) {
    cli::cli_abort(
      c(
        "Decomposition requires seasonal data ({.arg frequency} > 1).",
        "i" = "Annual series have no seasonal component to decompose.",
        "i" = "Use {.fn augment_trends} to extract a trend component from non-seasonal series."
      ),
      call = call
    )
  }

  # Warn about NA rows that will be excluded from decomposition
  n_na <- sum(!stats::complete.cases(data[[date_col]], data[[value_col]]))
  if (n_na > 0 && !.quiet) {
    cli::cli_inform(
      "{n_na} row{?s} with missing values excluded from decomposition (component columns will be {.val NA} for those rows)."
    )
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

  # Log transform: decompose additively on the log scale and exponentiate the
  # components back so the product identity holds. Requires positive values.
  use_log <- identical(transform, "log")
  if (use_log) {
    if (any(as.numeric(ts_data) <= 0, na.rm = TRUE)) {
      cli::cli_abort(
        c(
          "{.code transform = \"log\"} requires strictly positive values.",
          "i" = "Use {.code transform = \"none\"} for series with zero or negative values."
        ),
        call = call
      )
    }
    ts_fit <- log(ts_data)
  } else {
    ts_fit <- ts_data
  }

  # Decompose under each requested method, accumulating component columns.
  # Only leading and trailing missing values can reach this point, because
  # .df_to_ts_internal() aborts on interior gaps: dropping one would shift every
  # later observation and silently break the value = trend + seasonal +
  # remainder identity. The date-keyed merge therefore only has to absorb the
  # shorter fitted series, plus date convention differences (e.g. end-of-month
  # vs first-of-month), which .safe_merge() normalises with floor_date().
  result <- data
  for (m in methods) {
    components <- switch(
      m,
      stl = .decompose_stl(ts_fit, params, .quiet),
      regression = .decompose_regression(ts_fit, trend, params, .quiet),
      classic = .decompose_classic(ts_fit, .quiet),
      bsm = .decompose_bsm(ts_fit, .quiet, call),
      seats = .decompose_seats(ts_fit, .quiet, call)
    )

    # Components from a log-scale fit return to the original scale via exp(),
    # turning the additive identity into the product identity.
    if (use_log) {
      components <- lapply(components, .ts_exp)
    }

    components_df <- .decompose_components_to_df(
      components,
      m,
      date_col,
      seasadj,
      multiplicative = use_log
    )
    result <- .safe_merge(result, components_df, date_col, frequency)
  }

  return(result)
}

#' Exponentiate a ts component while preserving its time index.
#' @noRd
.ts_exp <- function(x) {
  stats::ts(
    exp(as.numeric(x)),
    start = stats::start(x),
    frequency = stats::frequency(x)
  )
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
  methods,
  trend,
  transform,
  frequency,
  seasadj,
  params,
  .quiet,
  call = rlang::caller_env()
) {
  # Unused factor levels produce empty groups, which would otherwise fail
  # downstream on an unrelated complete-cases check.
  data_split <- split(data, data[group_cols])
  data_split <- data_split[vapply(data_split, nrow, integer(1)) > 0]
  group_names <- names(data_split)

  if (length(data_split) == 0) {
    cli::cli_abort("No groups found for {.val {group_cols}}", call = call)
  }

  # Detect frequency once from the first group
  if (is.null(frequency)) {
    frequency <- .detect_frequency(data_split[[1]][[date_col]], .quiet = .quiet)
  }

  if (!.quiet) {
    cli::cli_inform(c(
      "Decomposing {length(group_names)} group(s) using {.val {methods}} method:",
      "i" = "Groups: {.val {group_names}}"
    ))
  }

  results <- lapply(data_split, function(group_data) {
    .decompose_series_single(
      data = group_data,
      date_col = date_col,
      value_col = value_col,
      methods = methods,
      trend = trend,
      transform = transform,
      frequency = frequency,
      seasadj = seasadj,
      params = params,
      .quiet = TRUE,
      call = call
    )
  })

  # Combine groups with base rbind (mirrors augment_trends()), keeping the
  # package free of a hard dplyr dependency. as_tibble() drops the row names
  # that rbind() attaches and restores the tibble class.
  result <- tibble::as_tibble(do.call(rbind, results))
  return(result)
}

# ---------------------------------------------------------------------------
# Internal: convert components list to a date-keyed data frame
# ---------------------------------------------------------------------------

#' Build a date-keyed tibble from the three decomposition components.
#' All three ts objects share the same time index, so a single ts_df() call
#' recovers the dates and the numeric vectors are extracted directly. When
#' `seasadj = TRUE`, a seasonally adjusted column is appended: the series with
#' the seasonal component removed (`trend + remainder` for additive
#' decompositions, `trend * remainder` for multiplicative ones).
#' @noRd
.decompose_components_to_df <- function(
  components,
  method,
  date_col,
  seasadj = FALSE,
  multiplicative = FALSE
) {
  dates_df <- tsbox::ts_df(components$trend)

  trend <- as.numeric(components$trend)
  seasonal <- as.numeric(components$seasonal)
  remainder <- as.numeric(components$remainder)

  result <- tibble::tibble(
    !!date_col := dates_df[[1]],
    !!paste0("trend_", method) := trend,
    !!paste0("seasonal_", method) := seasonal,
    !!paste0("remainder_", method) := remainder
  )

  if (seasadj) {
    seasadj_vals <- if (multiplicative) trend * remainder else trend + remainder
    result[[paste0("seasadj_", method)]] <- seasadj_vals
  }

  return(result)
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
    linear = 1L,
    quadratic = 2L,
    cubic = 3L
  )

  # Default: orthogonal polynomials (numerically stable, especially for degree >= 2)
  poly_raw <- params[["poly_raw"]] %||% FALSE

  freq <- stats::frequency(ts_data)
  values <- as.numeric(ts_data)
  t_idx <- as.numeric(stats::time(ts_data))

  # Period indicator: 1..freq (month for monthly, quarter for quarterly)
  period <- factor(as.integer(stats::cycle(ts_data)))

  if (!.quiet) {
    poly_type <- if (poly_raw) "raw" else "orthogonal"
    cli::cli_inform(
      "Computing regression decomposition: {trend} trend ({poly_type} polynomial, degree = {degree}) + {freq}-period dummies"
    )
  }

  # Joint model: polynomial trend + period dummy variables.
  # predict(type = "terms") returns each term's centred contribution plus a
  # "constant" attribute. Summing them recovers the fitted values, so splitting
  # the seasonal term from the trend term reproduces the data exactly:
  #   value = trend + seasonal + remainder.
  fit <- stats::lm(
    values ~ stats::poly(t_idx, degree = degree, raw = poly_raw) + period
  )

  term_contributions <- stats::predict(fit, type = "terms")
  fitted_vals <- attr(term_contributions, "constant") +
    rowSums(term_contributions)

  seasonal_vals <- as.numeric(term_contributions[, "period"])
  trend_vals <- fitted_vals - seasonal_vals
  remainder_vals <- values - fitted_vals

  .make_ts <- function(x) {
    stats::ts(x, start = stats::start(ts_data), frequency = freq)
  }

  return(list(
    trend = .make_ts(trend_vals),
    seasonal = .make_ts(seasonal_vals),
    remainder = .make_ts(remainder_vals)
  ))
}

# ---------------------------------------------------------------------------
# Internal: classical decomposition (stats::decompose)
# ---------------------------------------------------------------------------

#' @noRd
.decompose_classic <- function(ts_data, .quiet) {
  if (!.quiet) {
    cli::cli_inform("Computing classical decomposition (additive)")
  }

  dec <- stats::decompose(ts_data, type = "additive")

  # decompose() leaves NA at the first/last freq/2 points of the trend (and hence
  # the remainder) because the centred moving average has no boundary support.
  # Those NAs flow through to the output rows, which is the expected behaviour.
  return(list(
    trend = dec$trend,
    seasonal = dec$seasonal,
    remainder = dec$random
  ))
}

# ---------------------------------------------------------------------------
# Internal: Basic Structural Model (stats::StructTS, type = "BSM")
# ---------------------------------------------------------------------------

#' @noRd
.decompose_bsm <- function(ts_data, .quiet, call = rlang::caller_env()) {
  if (!.quiet) {
    cli::cli_inform(
      "Computing Basic Structural Model decomposition (Kalman smoother)"
    )
  }

  fit <- tryCatch(
    stats::StructTS(ts_data, type = "BSM"),
    error = function(e) {
      cli::cli_abort(
        c(
          "Basic Structural Model fitting failed.",
          "x" = conditionMessage(e),
          "i" = "BSM relies on numerical optimisation; try a longer series or a different method."
        ),
        call = call
      )
    }
  )

  # tsSmooth() returns the Kalman-smoothed states: columns "level", "slope",
  # and "sea" (the seasonal state). Trend = level, seasonal = sea.
  smoothed <- stats::tsSmooth(fit)

  freq <- stats::frequency(ts_data)
  ts_start <- stats::start(ts_data)
  .make_ts <- function(x) {
    stats::ts(as.numeric(x), start = ts_start, frequency = freq)
  }

  level <- smoothed[, "level"]
  seasonal <- smoothed[, "sea"]
  # remainder = y - level - seasonal guarantees the exact additive identity.
  remainder <- as.numeric(ts_data) - level - seasonal

  return(list(
    trend = .make_ts(level),
    seasonal = .make_ts(seasonal),
    remainder = .make_ts(remainder)
  ))
}

# ---------------------------------------------------------------------------
# Internal: X-13ARIMA-SEATS decomposition (seasonal::seas)
# ---------------------------------------------------------------------------

#' @noRd
.decompose_seats <- function(ts_data, .quiet, call = rlang::caller_env()) {
  if (!requireNamespace("seasonal", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "{.val seats} decomposition requires the {.pkg seasonal} package.",
        "i" = "Install it with {.run install.packages(\"seasonal\")}.",
        "i" = "{.pkg seasonal} bundles the X-13ARIMA-SEATS binaries via {.pkg x13binary}."
      ),
      call = call
    )
  }

  if (!.quiet) {
    cli::cli_inform("Computing X-13ARIMA-SEATS decomposition (SEATS)")
  }

  # seas() runs X-13ARIMA-SEATS with automatic model selection, transformation,
  # outlier detection and calendar adjustment - the standard production setup.
  fit <- tryCatch(
    seasonal::seas(ts_data),
    error = function(e) {
      cli::cli_abort(
        c(
          "X-13ARIMA-SEATS estimation failed.",
          "x" = conditionMessage(e),
          "i" = "X-13 needs at least 3 full years of monthly or quarterly data and a regular series."
        ),
        call = call
      )
    }
  )

  # Express the X-13 output in a common additive form so the identity
  # value = trend + seasonal + remainder holds exactly, regardless of whether
  # X-13 fitted a multiplicative or additive model:
  #   adj (s11) = seasonally adjusted series, trend (s12) = trend-cycle.
  #   seasonal  = value - adj  (seasonal + calendar + outlier effects removed)
  #   remainder = adj - trend  (the irregular component)
  comps <- tryCatch(
    list(
      adj = as.numeric(seasonal::series(fit, "s11")),
      trend_cycle = as.numeric(seasonal::series(fit, "s12"))
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not extract the SEATS components ({.val s11}/{.val s12}).",
          "x" = conditionMessage(e),
          "i" = "This can happen when X-13 falls back to X11 instead of SEATS."
        ),
        call = call
      )
    }
  )

  value <- as.numeric(ts_data)
  seasonal_comp <- value - comps$adj
  remainder <- comps$adj - comps$trend_cycle

  freq <- stats::frequency(ts_data)
  ts_start <- stats::start(ts_data)
  .make_ts <- function(x) stats::ts(x, start = ts_start, frequency = freq)

  return(list(
    trend = .make_ts(comps$trend_cycle),
    seasonal = .make_ts(seasonal_comp),
    remainder = .make_ts(remainder)
  ))
}

# ---------------------------------------------------------------------------
# Internal: validate params keys
# ---------------------------------------------------------------------------

#' @noRd
.validate_params_keys <- function(params, methods, .quiet) {
  if (length(params) == 0 || .quiet) {
    return(invisible(NULL))
  }

  key_map <- list(
    "stl" = c(
      "s.window",
      "stl_s_window",
      "t.window",
      "stl_t_window",
      "robust",
      "stl_robust"
    ),
    "regression" = "poly_raw",
    "classic" = character(0),
    "bsm" = character(0),
    "seats" = character(0)
  )

  # A key is unknown only if no requested method accepts it.
  valid_keys <- unique(unlist(key_map[methods], use.names = FALSE))

  unknown <- setdiff(names(params), valid_keys)
  n_unknown <- length(unknown)
  if (n_unknown > 0) {
    cli::cli_warn(
      "Unknown {.arg params} {cli::qty(n_unknown)} key{?s} for {.val {methods}} method: {.val {unknown}}"
    )
  }

  return(invisible(NULL))
}
