# detrend_series ----------------------------------------------------------

#' Detrend a time series
#'
#' @description
#' Pipe-friendly convenience wrapper around [augment_trends()] focused on a
#' single task: removing the trend from a time series. It adds a
#' `detrend_{method}` column holding the detrended series (the deviation from
#' trend, often called the *cycle* in economics) and, optionally, the
#' underlying trend itself.
#'
#' For econometric filters such as `"hp"` (the default), `"bk"`, `"cf"`, and
#' `"hamilton"`, the detrended series is the business-cycle component those
#' filters were designed to isolate (e.g. the output gap).
#'
#' @inheritParams augment_trends
#' @param value_col Name of the value column(s). Defaults to `"value"`. Must be
#'   numeric. A character vector of length > 1 is accepted; each column is
#'   detrended separately and the results are named `detrend_{method}_{col}`
#'   (e.g. `detrend_hp_consumption`).
#' @param methods Character vector of trend methods used for detrending. Any
#'   method supported by [augment_trends()] is accepted. Default is `"hp"`
#'   (Hodrick-Prescott filter with frequency-appropriate smoothing). When
#'   several methods are supplied, each one contributes its own
#'   `detrend_{method}` column so the detrended series can be compared side by
#'   side.
#' @param transform Transformation applied before detrending. One of:
#'   - `"none"` (default): the trend is fitted to the raw series and
#'     `detrend = value - trend`, in the units of the series.
#'   - `"log"`: the trend is fitted to the log series and
#'     `detrend = log(value) - log(trend)`, the *log deviation from trend*.
#'     Multiplied by 100, this is approximately the percentage deviation from
#'     trend (the convention for output gaps). Requires strictly positive
#'     values. The `trend_{method}` columns (when `components = TRUE`) are
#'     reported back in the units of the series.
#' @param window Unified window/period parameter for moving average methods;
#'   see [augment_trends()]. For `"ma"`, `"median"`, and `"henderson"`, a
#'   numeric vector is accepted (e.g. `c(6, 12)`), which adds one detrended
#'   column per window value (`detrend_ma_6`, `detrend_ma_12`, ...).
#' @param components If `FALSE` (default), only the detrended
#'   `detrend_{method}` column is added. If `TRUE`, the fitted
#'   `trend_{method}` column is also kept.
#'
#' @return A tibble with the original columns plus, for each requested method,
#'   a `detrend_{method}` column holding the detrended series. When
#'   `components = TRUE`, the `trend_{method}` column is kept as well.
#'
#'   Each detrended column mirrors the name of the trend column it derives
#'   from: window vectors yield `detrend_ma_6`, `detrend_ma_12`, and a trend
#'   column renamed to avoid a naming conflict yields a matching detrended
#'   name.
#'
#'   With `transform = "none"` the trend and the detrended series should add
#'   back up to the original (`value = trend + detrend`); with
#'   `transform = "log"` the relation is `value = trend * exp(detrend)`. Methods with
#'   boundary effects (e.g. `"bk"`, `"hamilton"`) produce `NA` trend values at
#'   the affected observations, and the detrended series is `NA` there too.
#'
#'   Output rows come back in the order they were supplied in.
#'
#' @details
#' `detrend_series()` is a thin wrapper: it calls [augment_trends()] with the
#' requested methods and subtracts each fitted trend from the series (on the
#' log scale when `transform = "log"`). All trend-fitting behaviour,
#' validation, grouping, and the unified parameters (`window`, `smoothing`,
#' `band`, `align`, `params`) are inherited unchanged from [augment_trends()].
#' See its documentation for method internals and parameter details.
#'
#' Detrending does **not** remove seasonality: the detrended series of a raw
#' seasonal series still contains the seasonal swings, and seasonality can
#' leak into the cycle estimated by filters such as HP. For seasonal data,
#' seasonally adjust first and detrend the adjusted series (see Examples), or
#' use [decompose_series()] for a full trend/seasonal/remainder split.
#'
#' @seealso [augment_trends()] for the underlying trend extraction and the
#'   full set of methods; [deseason_series()] to remove seasonality;
#'   [decompose_series()] for a full decomposition.
#'
#' @examples
#' # HP-filter detrending (the default): adds a detrend_hp column
#' gdp_construction |>
#'   detrend_series(value_col = "index")
#'
#' # Log deviation from trend (x 100 ~ percentage gap, the output-gap convention)
#' gdp_construction |>
#'   detrend_series(value_col = "index", transform = "log")
#'
#' # Keep the fitted trend alongside the detrended series
#' gdp_construction |>
#'   detrend_series(value_col = "index", components = TRUE)
#'
#' # Compare detrending methods side by side
#' gdp_construction |>
#'   detrend_series(value_col = "index", methods = c("hp", "stl", "loess"))
#'
#' # Seasonal data: deseason first, then detrend the adjusted series
#' gdp_construction |>
#'   deseason_series(value_col = "index") |>
#'   detrend_series(value_col = "seasadj_stl")
#'
#' # Grouped detrending: one trend per electricity sector
#' electricity |>
#'   detrend_series(group_cols = "name_series")
#'
#' @importFrom cli cli_abort cli_warn
#' @importFrom tibble as_tibble
#'
#' @export
detrend_series <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  methods = "hp",
  transform = "none",
  frequency = NULL,
  components = FALSE,
  window = NULL,
  smoothing = NULL,
  band = NULL,
  align = NULL,
  params = list(),
  .quiet = FALSE
) {
  # Validate the arguments this wrapper acts on itself; everything else is
  # validated by augment_trends().
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame, tibble, or data.table")
  }

  .check_non_empty(data)

  missing_value_cols <- setdiff(value_col, names(data))
  if (length(missing_value_cols) > 0) {
    cli::cli_abort("Column{?s} not found in data: {.val {missing_value_cols}}")
  }

  non_numeric <- value_col[!vapply(data[value_col], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    cli::cli_abort("Column{?s} must be numeric: {.val {non_numeric}}")
  }

  .validate_methods(methods)
  methods <- unique(methods)

  valid_transforms <- c("none", "log")
  if (length(transform) != 1 || !transform %in% valid_transforms) {
    cli::cli_abort(
      "Invalid transform {.val {transform}}. Valid options: {.val {valid_transforms}}"
    )
  }

  if (!is.logical(components) || length(components) != 1 || is.na(components)) {
    cli::cli_abort(
      "{.arg components} must be a single {.code TRUE} or {.code FALSE}"
    )
  }

  data <- tibble::as_tibble(data)

  # Detrend each value column in turn. With several columns, the column name
  # is appended as a suffix (detrend_{method}_{col}), mirroring augment_trends().
  result <- data
  for (vc in value_col) {
    result <- .detrend_series_col(
      data = result,
      date_col = date_col,
      value_col = vc,
      group_cols = group_cols,
      methods = methods,
      transform = transform,
      frequency = frequency,
      components = components,
      window = window,
      smoothing = smoothing,
      band = band,
      align = align,
      params = params,
      suffix = if (length(value_col) > 1) vc else NULL,
      .quiet = .quiet
    )
  }

  return(result)
}

## Internal: detrend a single value column ----------------------------------

#' Fit trends for one value column and subtract them from the series.
#'
#' With `transform = "log"`, the trend is fitted to a temporary log-scale
#' column so the original value column is never modified; the fitted trend is
#' exponentiated back to the units of the series after the log deviation is
#' computed. Trend columns are identified by diffing the column names before
#' and after the augment_trends() call (never by reconstructing expected
#' names), so conflict-renamed columns are handled correctly and pre-existing
#' user columns are never touched.
#' @noRd
.detrend_series_col <- function(
  data,
  date_col,
  value_col,
  group_cols,
  methods,
  transform,
  frequency,
  components,
  window,
  smoothing,
  band,
  align,
  params,
  suffix,
  .quiet
) {
  use_log <- identical(transform, "log")

  fit_col <- value_col
  fit_data <- data
  if (use_log) {
    if (any(data[[value_col]] <= 0, na.rm = TRUE)) {
      cli::cli_abort(c(
        "{.code transform = \"log\"} requires strictly positive values in {.val {value_col}}.",
        "i" = "Use {.code transform = \"none\"} for series with zero or negative values."
      ))
    }
    fit_col <- ".detrend_log_fit"
    while (fit_col %in% names(data)) {
      fit_col <- paste0(fit_col, "_")
    }
    fit_data[[fit_col]] <- log(data[[value_col]])
  }

  augmented <- augment_trends(
    data = fit_data,
    date_col = date_col,
    value_col = fit_col,
    group_cols = group_cols,
    methods = methods,
    frequency = frequency,
    suffix = suffix,
    window = window,
    smoothing = smoothing,
    band = band,
    align = align,
    params = params,
    .quiet = .quiet
  )

  # The columns actually added (post conflict-renaming) are the trend columns.
  trend_cols <- setdiff(names(augmented), names(fit_data))

  for (tc in trend_cols) {
    dc <- .detrend_unique_name(sub("^trend_", "detrend_", tc), names(augmented))
    augmented[[dc]] <- augmented[[fit_col]] - augmented[[tc]]
    if (use_log) {
      augmented[[tc]] <- exp(augmented[[tc]])
    }
  }

  if (use_log) {
    augmented[[fit_col]] <- NULL
  }

  if (!components) {
    augmented <- augmented[,
      setdiff(names(augmented), trend_cols),
      drop = FALSE
    ]
  }

  return(augmented)
}

#' Resolve a name conflict for a detrended column, mirroring .safe_merge()
#' @noRd
.detrend_unique_name <- function(name, existing) {
  .unique_column_name(name, existing, description = "detrended")
}
