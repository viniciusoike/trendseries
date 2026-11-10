#' Aggregate series to a lower frequency
#'
#' @description
#' Summarise daily, weekly, or monthly series into calendar months, quarters,
#' or years, such as the last daily close of each month or the quarterly sum
#' of a monthly flow.
#'
#' @param data A non-empty `data.frame`, `tibble`, or `data.table`.
#' @param date_col Name of the `Date` column. Defaults to `"date"`.
#' @param value_col Non-empty character vector naming numeric value columns.
#'   Each column is aggregated with the same `summary`.
#' @param group_cols Optional character vector naming grouping columns. Each
#'   group is aggregated separately. Grouping columns must differ from
#'   `date_col` and `value_col`.
#' @param period Target calendar period: `"month"`, `"quarter"`, or
#'   `"year"`.
#' @param summary Statistic computed within each period: `"last"`, `"first"`,
#'   `"mean"`, `"sum"`, `"max"`, or `"min"`. Use `"last"` or `"mean"` for
#'   stocks and prices, and `"sum"` for flows.
#' @param na_rm Whether to drop missing values before summarising. With the
#'   default `FALSE`, `"mean"`, `"sum"`, `"max"`, and `"min"` return `NA` for
#'   a period containing a missing value, while `"first"` and `"last"` return
#'   the value on the first or last dated row, which may be missing. With
#'   `TRUE`, they use the first or last observed value. A period with no
#'   observed values returns `NA` in either case.
#'
#' @return A tibble with one row per group and observed period, holding the
#'   grouping columns, the date column, and the aggregated value columns as
#'   doubles. Other columns are dropped. Rows are sorted by group and date.
#'
#' @details
#' Each period is dated by its first day, so January 2024 is dated
#' `2024-01-01` whatever the dates of the underlying observations. Only
#' periods with at least one observation are returned.
#'
#' Periods at the start and end of a series may be partly observed, and their
#' sums and means then cover fewer observations than complete periods. With
#' `summary = "last"`, the value is the last observation in the period, which
#' for trading data may fall before the last calendar day. A weekly
#' observation is assigned whole to the period containing its date, so a week
#' that straddles a month end is not split.
#'
#' Grouping comes only from `group_cols`. A grouped tibble or a keyed tsibble
#' needs its grouping variables passed there. A warning flags calls in which
#' no period holds more than one observation, which happens when `period` is
#' not coarser than the data.
#'
#' @seealso [interpolate_series()] to fill missing values before aggregating,
#'   and [augment_rolling()] for rolling and year-to-date aggregations at the
#'   original frequency.
#'
#' @examples
#' # Quarterly sum of monthly vehicle production
#' vehicles |>
#'   aggregate_series(
#'     value_col = "production",
#'     period = "quarter",
#'     summary = "sum"
#'   )
#'
#' # Annual average of each retail series
#' retail_volume |>
#'   aggregate_series(
#'     group_cols = "name_series",
#'     period = "year",
#'     summary = "mean"
#'   )
#'
#' @export
aggregate_series <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  period = c("month", "quarter", "year"),
  summary = c("last", "first", "mean", "sum", "max", "min"),
  na_rm = FALSE
) {
  period <- rlang::arg_match(period)
  summary <- rlang::arg_match(summary)
  group_cols <- if (is.null(group_cols)) NULL else unique(group_cols)

  # The index validator checks the same columns, missing dates, and repeated
  # dates within groups; its base-period arguments take their neutral values.
  group_indices <- .validate_index_args(
    data,
    date_col,
    value_col,
    group_cols,
    base_period = NULL,
    base_value = 100,
    na_rm = na_rm,
    suffix = NULL,
    .quiet = TRUE
  )
  .check_aggregate_roles(date_col, value_col, group_cols)

  data <- .as_plain_tibble(data)
  pieces <- lapply(group_indices, function(indices) {
    rows <- indices[order(data[[date_col]][indices])]
    .aggregate_group(
      data[rows, , drop = FALSE],
      date_col,
      value_col,
      group_cols,
      period,
      summary,
      na_rm
    )
  })

  result <- do.call(rbind, unname(pieces))
  if (nrow(data) > 1 && nrow(result) == nrow(data)) {
    cli::cli_warn(c(
      "No {period} holds more than one observation, so nothing was aggregated.",
      "i" = "Choose a {.arg period} coarser than the frequency of the data."
    ))
  }
  return(result)
}

# Aggregate one date-sorted group into its calendar periods.
.aggregate_group <- function(
  group_data,
  date_col,
  value_col,
  group_cols,
  period,
  summary,
  na_rm
) {
  periods <- lubridate::floor_date(group_data[[date_col]], unit = period)
  period_rows <- split(seq_along(periods), periods)

  first_rows <- vapply(period_rows, `[`, integer(1), 1)
  result <- group_data[first_rows, group_cols, drop = FALSE]
  result[[date_col]] <- as.Date(names(period_rows))

  for (value in value_col) {
    values <- group_data[[value]]
    result[[value]] <- vapply(
      period_rows,
      function(rows) .aggregate_values(values[rows], summary, na_rm),
      numeric(1),
      USE.NAMES = FALSE
    )
  }
  return(result)
}

# A grouping column that is also the date or a value would be overwritten in
# the output, merging groups that should stay apart.
.check_aggregate_roles <- function(
  date_col,
  value_col,
  group_cols,
  call = rlang::caller_env()
) {
  overlap <- intersect(group_cols, c(date_col, value_col))
  if (length(overlap) > 0) {
    cli::cli_abort(
      c(
        "{.arg group_cols} must not include {.arg date_col} or {.arg value_col}.",
        "x" = "Also used as date or value: {.val {overlap}}"
      ),
      call = call
    )
  }
  return(invisible(NULL))
}

.aggregate_values <- function(x, summary, na_rm) {
  if (na_rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    return(NA_real_)
  }
  value <- switch(
    summary,
    first = x[1],
    last = x[length(x)],
    mean = mean(x),
    sum = sum(x),
    max = max(x),
    min = min(x)
  )
  return(as.numeric(value))
}
