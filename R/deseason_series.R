#' Seasonally adjust (deseason) a time series
#'
#' @description
#' Pipe-friendly convenience wrapper around [decompose_series()] focused on a
#' single task: removing the seasonal component from a time series. It adds a
#' `seasadj_{method}` column holding the seasonally adjusted (deseasoned) series
#' and, optionally, the underlying trend, seasonal, and remainder components.
#'
#' @inheritParams decompose_series
#' @param methods Seasonal-adjustment method(s). One or more of `"stl"` (default) or
#'   `"seats"`. When both are supplied, each contributes its own
#'   `seasadj_{method}` column (and component columns when `components = TRUE`)
#'   so the adjustments can be compared side by side.
#'   - `"stl"`: Seasonal-Trend decomposition via Loess (`stats::stl()`).
#'   - `"seats"`: X-13ARIMA-SEATS decomposition (requires the **`seasonal`**
#'     package; see [decompose_series()] for details).
#' @param components If `FALSE` (default), only the seasonally adjusted
#'   `seasadj_{method}` column is added. If `TRUE`, the `trend_{method}`,
#'   `seasonal_{method}`, and `remainder_{method}` columns are also added (the
#'   full [decompose_series()] output).
#'
#' @return A tibble with the original columns plus, for each requested method, a
#'   `seasadj_{method}` column holding the seasonally adjusted series. When
#'   `components = TRUE`, the `trend_{method}`, `seasonal_{method}`, and
#'   `remainder_{method}` columns are added as well.
#'
#'   The seasonally adjusted series is the series with the seasonal component
#'   removed: `trend + remainder` for additive decompositions, `trend *
#'   remainder` when `transform = "log"`. Output rows are ordered by date within
#'   each group; the original row order is not preserved.
#'
#' @details
#' `deseason_series()` is a thin wrapper: it calls [decompose_series()] with
#' `seasadj = TRUE` and then keeps only the seasonally adjusted column unless
#' `components = TRUE`. All seasonal-adjustment behaviour, validation, grouping,
#' and the `transform = "log"` (multiplicative) path are inherited unchanged
#' from [decompose_series()]. See its documentation for method internals and the
#' meaning of the `params` argument.
#'
#' For a full trend/seasonal/remainder decomposition, or for the `regression`,
#' `classic`, and `bsm` methods, use [decompose_series()] directly.
#'
#' @seealso [decompose_series()] for the underlying decomposition and the full
#'   set of methods; [augment_trends()] to extract a trend component only.
#'
#' @examples
#' # Seasonally adjust a quarterly series (STL, the default)
#' gdp_construction |>
#'   deseason_series(value_col = "index")
#'
#' # Also keep the trend, seasonal, and remainder components
#' gdp_construction |>
#'   deseason_series(value_col = "index", components = TRUE)
#'
#' # Multiplicative adjustment via log transform (seasonal swings grow with level)
#' gdp_construction |>
#'   deseason_series(value_col = "index", transform = "log")
#'
#' # X-13ARIMA-SEATS adjustment (requires the 'seasonal' package)
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   gdp_construction |>
#'     deseason_series(value_col = "index", methods = "seats")
#' }
#'
#' # Compare STL and SEATS adjustments side by side
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   gdp_construction |>
#'     deseason_series(value_col = "index", methods = c("stl", "seats"))
#' }
#'
#' # Grouped seasonal adjustment: one adjustment per electricity sector
#' electricity |>
#'   deseason_series(group_cols = "name_series")
#'
#' @importFrom cli cli_abort
#'
#' @export
deseason_series <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  methods = "stl",
  transform = "none",
  frequency = NULL,
  components = FALSE,
  params = list(),
  .quiet = FALSE
) {
  # Restrict to the methods suited to seasonal adjustment; decompose_series()
  # validates the remaining arguments.
  valid_methods <- c("stl", "seats")
  if (
    !is.character(methods) ||
      length(methods) < 1 ||
      !all(methods %in% valid_methods)
  ) {
    bad <- setdiff(methods, valid_methods)
    cli::cli_abort(c(
      "Invalid methods: {.val {bad}}. Valid options: {.val {valid_methods}}.",
      "i" = "For other methods, use {.fn decompose_series} directly."
    ))
  }
  methods <- unique(methods)

  if (!is.logical(components) || length(components) != 1 || is.na(components)) {
    cli::cli_abort(
      "{.arg components} must be a single {.code TRUE} or {.code FALSE}"
    )
  }

  # Decompose, always requesting the seasonally adjusted column.
  result <- decompose_series(
    data = data,
    date_col = date_col,
    value_col = value_col,
    group_cols = group_cols,
    methods = methods,
    transform = transform,
    frequency = frequency,
    seasadj = TRUE,
    params = params,
    .quiet = .quiet
  )

  # Drop the component columns unless the caller asked to keep them. Use the
  # columns decompose_series() actually added (post conflict-renaming) rather
  # than reconstructed names, so pre-existing user columns are never dropped.
  if (!components) {
    added_cols <- setdiff(names(result), names(data))
    drop_cols <- added_cols[!startsWith(added_cols, "seasadj_")]
    result <- result[, setdiff(names(result), drop_cols), drop = FALSE]
  }

  return(result)
}
