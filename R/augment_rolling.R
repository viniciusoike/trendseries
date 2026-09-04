#' Add rolling aggregation columns to a data frame
#'
#' @description
#' Pipe-friendly companion to [augment_trends()] for rolling and year-to-date
#' aggregations: 12-month accumulated totals, compounded rates of change,
#' rolling volatility, and so on. Columns are prefixed `roll_` rather than
#' `trend_`, because these are aggregations of the series and not estimates of
#' its trend.
#'
#' @param data A `data.frame`, `tibble`, or `data.table` containing the time
#'   series data.
#' @param date_col Name of the date column. Defaults to `"date"`. Must be of
#'   class `Date`.
#' @param value_col Name of the value column(s). Defaults to `"value"`. Must be
#'   `numeric`. A character vector of length > 1 is accepted; aggregations are
#'   computed for each column and named `roll_{stat}_{window}_{col}`.
#' @param group_cols Optional grouping variables for multiple time series. Can
#'   be a character vector of column names.
#' @param stats Character vector of rolling statistics. Options: `"sum"`
#'   (rolling total of flows), `"chain"` (compound accumulation of rates,
#'   `prod(1 + r) - 1`), `"mean"`, `"sd"`, `"min"`, `"max"`. Default is
#'   `"sum"`.
#' @param window Window length in periods. If `NULL`, defaults to the detected
#'   frequency (12 for monthly, 4 for quarterly). A numeric vector adds one
#'   column per window value. Alternatively, the string `"ytd"` computes an
#'   expanding year-to-date accumulation that resets each January (or Q1).
#'   Numeric windows and `"ytd"` cannot be mixed in one call.
#' @param frequency The frequency of the series. Supports 4 (quarterly) or 12
#'   (monthly). Auto-detected if not specified.
#' @param align Alignment of the window relative to the output position:
#'   `"right"` (default), `"center"`, or `"left"`. Ignored when `window = "ytd"`.
#'   An even window has no exact centre; see [roll_series()] for how each
#'   statistic handles that.
#' @param percent Only used by `stats = "chain"`. If `FALSE` (default), rates
#'   are assumed to be decimals (0.005 for 0.5%). If `TRUE`, rates are assumed
#'   to be percentages (0.5 for 0.5%) and the result is returned in percent.
#' @param na_rm If `TRUE`, missing values are ignored within each window. The
#'   default `FALSE` propagates `NA`, so an incomplete window yields `NA`. A
#'   window holding no observed values yields `NA` either way, as does a
#'   window holding one value for `"sd"`.
#' @param suffix Optional suffix appended to the generated column names.
#' @param .quiet If `TRUE`, suppress informational messages.
#'
#' @return A tibble with the original data plus rolling columns named
#'   `roll_{stat}_{window}` (e.g. `roll_sum_12`, `roll_chain_ytd`), with
#'   `_{suffix}` appended when `suffix` is supplied. Rows come back in the
#'   order they were supplied in.
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom tibble as_tibble
#'
#' @details
#' Use `"sum"` for flows measured in levels and `"chain"` for series that are
#' already rates of change. Summing monthly inflation rates approximates the
#' 12-month accumulation but is not equal to it; `"chain"` compounds them
#' correctly. See [roll_series()] for the underlying computation.
#'
#' `"mean"` overlaps with the simple moving average available through
#' `augment_trends(methods = "ma")`. The two differ in defaults rather than in
#' substance: rolling aggregations default to right alignment, while the moving
#' average trend defaults to centred alignment. Given the same window and
#' alignment they agree, including the 2xN correction for even centred
#' windows.
#'
#' Rows whose value is `NA` are kept in place, so window positions stay aligned
#' with the calendar; `na_rm` then decides whether such a window yields `NA` or
#' is computed from the observations that are present. Unlike
#' [augment_trends()], which rejects gaps inside the observed range, a rolling
#' window has well-defined local behaviour for a gap, so these functions accept
#' one. A period that is absent
#' from the data altogether cannot be positioned, so it raises an error rather
#' than shifting later observations — add the missing rows with an `NA` value
#' first.
#'
#' @seealso [roll_series()] for the time series interface, [augment_trends()]
#'   for trend estimation.
#'
#' @examples
#' # 12-month accumulated vehicle production
#' vehicles |> augment_rolling(value_col = "production", window = 12)
#'
#' # Several windows at once
#' vehicles |>
#'   tail(60) |>
#'   augment_rolling(value_col = "production", window = c(3, 6, 12))
#'
#' # Rolling mean and volatility side by side
#' ibcbr |>
#'   augment_rolling(value_col = "index", stats = c("mean", "sd"), window = 12)
#'
#' # Year-to-date accumulation, resetting each January
#' vehicles |> augment_rolling(value_col = "production", window = "ytd")
#'
#' # Grouped series
#' retail_volume |>
#'   augment_rolling(group_cols = "name_series", window = 12)
#'
#' @export
augment_rolling <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  stats = "sum",
  window = NULL,
  frequency = NULL,
  align = "right",
  percent = FALSE,
  na_rm = FALSE,
  suffix = NULL,
  .quiet = FALSE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame, tibble, or data.table")
  }

  .check_non_empty(data)

  if (!date_col %in% names(data)) {
    cli::cli_abort("Column {.val {date_col}} not found in data")
  }

  missing_value_cols <- setdiff(value_col, names(data))
  if (length(missing_value_cols) > 0) {
    cli::cli_abort("Column{?s} not found in data: {.val {missing_value_cols}}")
  }

  if (!inherits(data[[date_col]], "Date")) {
    cli::cli_abort("Column {.val {date_col}} must be of class Date")
  }

  non_numeric <- value_col[!vapply(data[value_col], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    cli::cli_abort("Column{?s} must be numeric: {.val {non_numeric}}")
  }

  if (!is.null(group_cols)) {
    if (!is.character(group_cols)) {
      cli::cli_abort("{.arg group_cols} must be a character vector")
    }
    missing_group_cols <- setdiff(group_cols, names(data))
    if (length(missing_group_cols) > 0) {
      cli::cli_abort(c(
        "Group variables not found in data: {.val {missing_group_cols}}.",
        "i" = "Available columns: {.val {names(data)}}"
      ))
    }
  }

  data <- tibble::as_tibble(data)

  # Several value columns: recurse once per column, using the column name as a
  # suffix so results are named roll_{stat}_{window}_{col}
  if (length(value_col) > 1) {
    result <- data
    for (vc in value_col) {
      vc_suffix <- if (is.null(suffix)) vc else paste0(vc, "_", suffix)
      result <- augment_rolling(
        result,
        date_col = date_col,
        value_col = vc,
        group_cols = group_cols,
        stats = stats,
        window = window,
        frequency = frequency,
        align = align,
        percent = percent,
        na_rm = na_rm,
        suffix = vc_suffix,
        .quiet = .quiet
      )
    }
    return(result)
  }

  if (is.null(group_cols)) {
    result <- .augment_rolling_single(
      data = data,
      date_col = date_col,
      value_col = value_col,
      stats = stats,
      window = window,
      frequency = frequency,
      align = align,
      percent = percent,
      na_rm = na_rm,
      suffix = suffix,
      .quiet = .quiet
    )
  } else {
    result <- .augment_rolling_grouped(
      data = data,
      date_col = date_col,
      value_col = value_col,
      group_cols = group_cols,
      stats = stats,
      window = window,
      frequency = frequency,
      align = align,
      percent = percent,
      na_rm = na_rm,
      suffix = suffix,
      .quiet = .quiet
    )
  }

  return(result)
}

#' Rolling aggregation for a single ungrouped series
#' @noRd
.augment_rolling_single <- function(
  data,
  date_col,
  value_col,
  stats,
  window,
  frequency,
  align,
  percent,
  na_rm,
  suffix,
  .quiet,
  .check_inputs = TRUE
) {
  if (is.null(frequency)) {
    frequency <- .detect_frequency(data[[date_col]], .quiet = .quiet)
  }

  if (frequency < 1 || frequency > 365) {
    cli::cli_abort(
      "Frequency must be between 1 (annual) and 365 (daily), got {frequency}"
    )
  }

  # Missing values are kept so window positions stay aligned with the calendar;
  # `na_rm` decides how each window handles them
  ts_data <- .df_to_ts_preserve_na(data, date_col, value_col, frequency)

  # The list form always carries the {stat}_{window} names that become column
  # names, so nothing has to rebuild them here
  rolled <- .roll_series_list(
    ts_data = ts_data,
    stats = stats,
    window = window,
    align = align,
    percent = percent,
    na_rm = na_rm,
    .quiet = .quiet,
    .check_inputs = .check_inputs
  )

  rolled_df <- .trends_to_df(
    rolled,
    date_col,
    suffix,
    prefix = "roll_",
    time_base = .time_base(ts_data)
  )
  result <- .safe_merge(data, rolled_df, date_col, frequency)

  return(result)
}

#' Reject groups shorter than the requested window, naming every one
#'
#' @description Left to the per-group calls, the first group short enough to
#' fail aborts the whole run with a message that names no group at all.
#' @noRd
.check_group_lengths <- function(data_split, window) {
  if (is.null(window) || identical(window, "ytd")) {
    return(invisible(NULL))
  }

  sizes <- vapply(data_split, nrow, integer(1))
  short <- sizes < max(window)
  if (!any(short)) {
    return(invisible(NULL))
  }

  offenders <- paste0(names(sizes)[short], " (", sizes[short], ")")

  cli::cli_abort(c(
    "Rolling window ({max(window)}) cannot exceed the number of observations in a group.",
    "x" = "Too short: {.val {offenders}}",
    "i" = "Each label shows the group and how many rows it has."
  ))
}

#' Rolling aggregation applied group by group
#' @noRd
.augment_rolling_grouped <- function(
  data,
  date_col,
  value_col,
  group_cols,
  stats,
  window,
  frequency,
  align,
  percent,
  na_rm,
  suffix,
  .quiet
) {
  # Split row positions rather than the data, so results return to their own
  # rows. Unused factor levels produce empty groups, which would otherwise fail
  # downstream on an unrelated complete-cases check.
  group_indices <- .index_group_indices(data, group_cols)
  data_split <- lapply(group_indices, function(rows) data[rows, , drop = FALSE])
  group_names <- names(group_indices)

  if (length(group_indices) == 0) {
    cli::cli_abort("No groups found for {.val {group_cols}}")
  }

  if (is.null(frequency)) {
    frequency <- .detect_frequency(data_split[[1]][[date_col]], .quiet = .quiet)
  }

  .check_group_lengths(data_split, window)

  # Run the argument and scale checks once on the whole input. Left to the
  # per-group calls they would either repeat for every group or, because those
  # calls are quiet, never run at all.
  .warn_ignored_args(stats, window, align, percent)
  .validate_chain_scale(data[[value_col]], stats, percent)
  if (identical(window, "ytd")) {
    start_date <- min(data[[date_col]], na.rm = TRUE)
    .warn_ytd_partial_start(
      .start_period(start_date, frequency),
      frequency,
      start_date
    )
  }

  if (!.quiet) {
    cli::cli_inform(c(
      "Computing {length(stats)} statistic{?s} for {length(group_names)} group{?s}:",
      "i" = "Statistics: {.val {stats}}",
      "i" = "Groups: {.val {group_names}}"
    ))
  }

  results <- lapply(data_split, function(group_data) {
    .augment_rolling_single(
      data = group_data,
      date_col = date_col,
      value_col = value_col,
      stats = stats,
      window = window,
      frequency = frequency,
      align = align,
      percent = percent,
      na_rm = na_rm,
      suffix = suffix,
      .quiet = TRUE,
      .check_inputs = FALSE
    )
  })

  # dplyr is Suggests-only, so groups are recombined with vctrs
  result <- vctrs::vec_rbind(!!!results)
  result <- result[order(unlist(group_indices, use.names = FALSE)), ]

  return(tibble::as_tibble(result))
}
