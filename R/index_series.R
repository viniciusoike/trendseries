#' Index one or more series
#'
#' @description
#' Rescale numeric series relative to their earliest observed value or to the
#' arithmetic mean over a selected base period.
#'
#' @param data A non-empty `data.frame`, `tibble`, or `data.table`.
#' @param date_col Name of the `Date` column. Defaults to `"date"`.
#' @param value_col Non-empty character vector naming numeric value columns.
#' @param group_cols Optional character vector naming grouping columns. Each
#'   group receives its own reference value.
#' @param base_period `NULL` to use the earliest non-missing observation; one
#'   or two four-digit integer years; or one or two `Date` values. Two values
#'   define an inclusive range and may be supplied in either order. A single
#'   date selects the calendar period containing it at the detected frequency.
#' @param base_value Finite positive number assigned to the reference. Defaults
#'   to 100.
#' @param na_rm Whether to remove missing values when averaging an explicit
#'   base period. With the default `FALSE`, a missing base observation raises
#'   an error instead of blanking the entire indexed series. This argument has
#'   no effect when `base_period = NULL`.
#' @param suffix Optional non-missing character suffix for generated names.
#' @param .quiet If `TRUE`, suppress frequency-detection messages. Warnings
#'   about incomplete base periods are never suppressed.
#'
#' @return A tibble containing the original columns, in their original order,
#'   followed by `index_{value_col}` columns (and `_{suffix}` when supplied).
#'   Unlike [augment_trends()] and [augment_rolling()], rows remain in their
#'   input order.
#'
#' @details
#' When `base_period` is supplied, dates are matched at the detected calendar
#' frequency. Thus, for monthly data, `as.Date("2019-01-01")` also matches an
#' observation dated at month end. Weekly and daily series use exact interval
#' containment. A partly observed base interval produces a warning.
#'
#' @seealso [augment_trends()] for trend estimation and [augment_rolling()] for
#'   rolling and year-to-date aggregations.
#'
#' @examples
#' vehicles |>
#'   index_series(value_col = "production")
#'
#' retail_volume |>
#'   index_series(group_cols = "name_series", base_period = 2019)
#'
#' @export
index_series <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  base_period = NULL,
  base_value = 100,
  na_rm = FALSE,
  suffix = NULL,
  .quiet = FALSE
) {
  group_indices <- .validate_index_args(
    data,
    date_col,
    value_col,
    group_cols,
    base_period,
    base_value,
    na_rm,
    suffix,
    .quiet
  )

  data <- tibble::as_tibble(data)
  frequency <- NULL
  period <- NULL
  if (!is.null(base_period)) {
    frequency <- .detect_frequency(data[[date_col]], .quiet = .quiet)
    period <- .resolve_base_period(base_period, frequency)
    .check_index_period_coverage(
      data,
      date_col,
      group_cols,
      group_indices,
      period
    )
  }

  for (value in value_col) {
    result <- rep(NA_real_, nrow(data))
    for (indices in group_indices) {
      group_data <- data[indices, , drop = FALSE]
      label <- .index_group_label(group_data, group_cols)
      result[indices] <- .index_one_series(
        dates = group_data[[date_col]],
        values = group_data[[value]],
        period = period,
        base_value = base_value,
        na_rm = na_rm,
        label = label,
        value_col = value
      )
    }

    proposed <- paste0(
      "index_",
      value,
      if (is.null(suffix)) "" else paste0("_", suffix)
    )
    output_name <- .unique_column_name(proposed, names(data), "indexed")
    data[[output_name]] <- result
  }

  return(data)
}

.validate_index_args <- function(
  data,
  date_col,
  value_col,
  group_cols,
  base_period,
  base_value,
  na_rm,
  suffix,
  .quiet
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame, tibble, or data.table")
  }
  .check_non_empty(data)
  if (!is.character(date_col) || length(date_col) != 1 || is.na(date_col)) {
    cli::cli_abort("{.arg date_col} must be one non-missing character value")
  }
  if (!date_col %in% names(data)) {
    cli::cli_abort("Column {.val {date_col}} not found in data")
  }
  if (!inherits(data[[date_col]], "Date")) {
    cli::cli_abort("Column {.val {date_col}} must be of class Date")
  }
  if (anyNA(data[[date_col]])) {
    cli::cli_abort("Column {.val {date_col}} must not contain missing dates")
  }
  if (!is.character(value_col) || length(value_col) == 0 || anyNA(value_col)) {
    cli::cli_abort("{.arg value_col} must be a non-empty character vector")
  }
  missing_values <- setdiff(value_col, names(data))
  if (length(missing_values) > 0) {
    cli::cli_abort("Column{?s} not found in data: {.val {missing_values}}")
  }
  non_numeric <- value_col[!vapply(data[value_col], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    cli::cli_abort("Column{?s} must be numeric: {.val {non_numeric}}")
  }
  if (!is.null(group_cols)) {
    if (
      !is.character(group_cols) || length(group_cols) == 0 || anyNA(group_cols)
    ) {
      cli::cli_abort(
        "{.arg group_cols} must be a non-empty character vector or NULL"
      )
    }
    missing_groups <- setdiff(group_cols, names(data))
    if (length(missing_groups) > 0) {
      cli::cli_abort(
        "Group variables not found in data: {.val {missing_groups}}"
      )
    }
  }
  if (!is.null(base_period)) {
    valid_date <- inherits(base_period, "Date") && length(base_period) %in% 1:2
    valid_year <- is.numeric(base_period) &&
      length(base_period) %in% 1:2 &&
      !anyNA(base_period) &&
      all(is.finite(base_period)) &&
      all(base_period == floor(base_period)) &&
      all(base_period >= 1000L & base_period <= 9999L)
    if (!valid_date && !valid_year) {
      cli::cli_abort(
        "{.arg base_period} must be NULL, one or two four-digit integers, or one or two Date values"
      )
    }
    if (valid_date && anyNA(base_period)) {
      cli::cli_abort("{.arg base_period} must not contain missing dates")
    }
  }
  if (
    !is.numeric(base_value) ||
      length(base_value) != 1 ||
      is.na(base_value) ||
      !is.finite(base_value) ||
      base_value <= 0
  ) {
    cli::cli_abort("{.arg base_value} must be one finite, positive number")
  }
  if (!is.logical(na_rm) || length(na_rm) != 1 || is.na(na_rm)) {
    cli::cli_abort("{.arg na_rm} must be one non-missing logical value")
  }
  if (
    !is.null(suffix) &&
      (!is.character(suffix) || length(suffix) != 1 || is.na(suffix))
  ) {
    cli::cli_abort(
      "{.arg suffix} must be one non-missing character value or NULL"
    )
  }
  if (!is.logical(.quiet) || length(.quiet) != 1 || is.na(.quiet)) {
    cli::cli_abort("{.arg .quiet} must be one non-missing logical value")
  }

  if (!is.null(base_period) && nrow(data) < 2) {
    cli::cli_abort(c(
      "{.arg data} must have at least two dated observations when {.arg base_period} is supplied.",
      "i" = "At least two dates are needed to detect the series frequency."
    ))
  }

  group_indices <- if (is.null(group_cols)) {
    list(seq_len(nrow(data)))
  } else {
    .index_group_indices(data, group_cols)
  }
  has_duplicates <- vapply(
    group_indices,
    function(i) anyDuplicated(data[[date_col]][i]) > 0,
    logical(1)
  )
  if (any(has_duplicates)) {
    cli::cli_abort(c(
      "Dates must not be duplicated within a series or group.",
      "i" = "If the data contains stacked series, identify them with {.arg group_cols}."
    ))
  }
  return(group_indices)
}

#' Split row positions by group, keeping the group labels
#'
#' @description Names each element with its group label, so callers can say
#' which group a message belongs to. `addNA()` keeps rows with a missing group
#' value instead of dropping them, which `split()` on the raw columns would do.
#' The split runs on the level codes rather than the factor itself, because
#' `split()` also drops a level whose label is `NA`.
#' @noRd
.index_group_indices <- function(data, group_cols) {
  grouping <- lapply(data[group_cols], function(x) addNA(as.factor(x)))
  keys <- do.call(
    interaction,
    c(grouping, list(drop = TRUE, lex.order = TRUE, sep = "."))
  )
  indices <- split(seq_len(nrow(data)), as.integer(keys), drop = TRUE)
  names(indices) <- levels(keys)[as.integer(names(indices))]

  return(indices)
}

.resolve_base_period <- function(base_period, frequency) {
  if (is.numeric(base_period) && !inherits(base_period, "Date")) {
    years <- sort(base_period)
    bounds <- as.Date(c(
      paste0(years[1], "-01-01"),
      paste0(years[length(years)], "-12-31")
    ))
  } else {
    bounds <- sort(base_period)
    if (length(bounds) == 1) bounds <- rep(bounds, 2)
  }

  unit <- .frequency_unit(frequency)
  if (!is.null(unit)) {
    bounds <- lubridate::floor_date(bounds, unit = unit)
  }
  return(list(start = bounds[1], end = bounds[2], unit = unit))
}

.index_one_series <- function(
  dates,
  values,
  period,
  base_value,
  na_rm,
  label,
  value_col
) {
  if (is.null(period)) {
    ordered <- values[order(dates)]
    observed <- ordered[!is.na(ordered)]
    if (length(observed) == 0) {
      cli::cli_abort(
        "Series {.val {value_col}} in {label} has no observed reference value"
      )
    }
    reference <- observed[1]
  } else {
    comparison_dates <- if (is.null(period$unit)) {
      dates
    } else {
      lubridate::floor_date(dates, unit = period$unit)
    }
    selected <- comparison_dates >= period$start &
      comparison_dates <= period$end
    if (!any(selected)) {
      cli::cli_abort(
        "Series {.val {value_col}} in {label} has no observations in the requested base period"
      )
    }
    base_values <- values[selected]
    if (all(is.na(base_values))) {
      cli::cli_abort(
        "Series {.val {value_col}} in {label} has only missing values in the base period"
      )
    }
    if (!na_rm && anyNA(base_values)) {
      cli::cli_abort(c(
        "Series {.val {value_col}} in {label} has missing values in the base period.",
        "i" = "Use {.code na_rm = TRUE} to average the available observations."
      ))
    }
    reference <- mean(base_values, na.rm = na_rm)
  }

  if (!is.finite(reference) || reference == 0) {
    cli::cli_abort(
      "Reference for series {.val {value_col}} in {label} must be finite and non-zero"
    )
  }
  return(values / reference * base_value)
}

.check_index_period_coverage <- function(
  data,
  date_col,
  group_cols,
  group_indices,
  period
) {
  for (indices in group_indices) {
    group_data <- data[indices, , drop = FALSE]
    comparison_dates <- if (is.null(period$unit)) {
      group_data[[date_col]]
    } else {
      lubridate::floor_date(group_data[[date_col]], unit = period$unit)
    }
    selected <- comparison_dates >= period$start &
      comparison_dates <= period$end
    if (any(selected)) {
      .warn_incomplete_index_period(
        comparison_dates[selected],
        period,
        .index_group_label(group_data, group_cols)
      )
    }
  }
  return(invisible(NULL))
}

.warn_incomplete_index_period <- function(dates, period, label) {
  if (is.null(period$unit)) {
    return(invisible(NULL))
  }
  expected <- seq.Date(period$start, period$end, by = period$unit)
  observed <- unique(dates)
  missing <- setdiff(expected, observed)
  if (length(missing) > 0) {
    cli::cli_warn(
      "Base period is incomplete for {label}: {length(missing)} expected observation{?s} {?is/are} missing."
    )
  }
  return(invisible(NULL))
}

.index_group_label <- function(data, group_cols) {
  if (is.null(group_cols)) {
    return("the ungrouped series")
  }
  values <- vapply(
    data[group_cols],
    function(x) as.character(x[1]),
    character(1)
  )
  return(paste0(
    "group ",
    paste0(group_cols, " = ", values, collapse = ", ")
  ))
}
