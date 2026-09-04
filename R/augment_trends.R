#' Add trend columns to data frame
#'
#' @description
#' Pipe-friendly function that adds trend columns to a tibble or data.frame.
#' Designed for exploratory analysis of monthly and quarterly economic time series.
#' Supports multiple trend extraction methods and handles grouped data.
#'
#' @param data A `data.frame`, `tibble`, or `data.table` containing the time series data.
#' @param date_col Name of the date column. Defaults to `"date"`.
#'   Must be of class `Date`.
#' @param value_col Name of the value column(s). Defaults to `"value"`.
#'   Must be `numeric`. A character vector of length > 1 is accepted; trends are
#'   extracted for each column and named `trend_{method}_{col}` (e.g.
#'   `trend_stl_consumption`).
#' @param group_cols Optional grouping variables for multiple
#'   time series. Can be a character vector of column names.
#' @param group_vars Deprecated. Use `group_cols` instead.
#' @param methods Character vector of trend methods.
#'   Options: `"hp"`, `"bk"`, `"cf"`, `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`,
#'   `"bn"`, `"ucm"`, `"hamilton"`, `"spencer"`, `"henderson"`, `"ewma"`, `"wma"`,
#'   `"triangular"`, `"kernel"`, `"kalman"`, `"median"`, `"gaussian"`.
#'   Default is `"stl"`.
#' @param frequency The frequency of the series.
#'   Supports 4 (quarterly) or 12 (monthly). Will be auto-detected if not specified.
#' @param suffix Optional suffix for trend column names.
#'   If NULL, uses method names.
#' @param window Unified window/period parameter for moving
#'   average methods (ma, wma, triangular, stl, ewma, median, gaussian). Must be positive.
#'   If NULL, uses frequency-appropriate defaults. For EWMA, the window is
#'   converted to the smoothing factor via `alpha = 2 / (window + 1)`. Cannot be
#'   used simultaneously with `smoothing` for EWMA method.
#'   For `ma`, `median`, and `henderson` methods, a numeric vector is accepted
#'   (e.g., `c(9, 13, 23)`), which adds one column per window value named
#'   `trend_henderson_9`, `trend_henderson_13`, etc. Other methods ignore extra
#'   values (with a warning).
#' @param smoothing Unified smoothing parameter for smoothing
#'   methods (hp, loess, spline, ewma, kernel, kalman).
#'   For hp: use large values (1600+) or small values (0-1) that get converted.
#'   For EWMA: specifies the alpha parameter (0-1) for traditional exponential smoothing.
#'   Cannot be used simultaneously with `window` for EWMA method.
#'   For kernel: multiplier of optimal bandwidth (1.0 = optimal, <1 = less smooth, >1 = more smooth).
#'   For kalman: controls the ratio of measurement to process noise (higher = more smoothing).
#'   For others: typically 0-1 range.
#' @param band Unified band parameter for bandpass filters
#'   (bk, cf). Both values must be positive.
#'   Provide as `c(low, high)` where low/high are periods in quarters, e.g., `c(6, 32)`.
#' @param align Unified alignment parameter for moving average
#'   methods (ma, wma, triangular, gaussian). Valid values: `"center"` (default, uses
#'   surrounding values), `"right"` (causal, uses past values only), `"left"` (anti-causal,
#'   uses future values only). Note: triangular only supports `"center"` and `"right"`.
#'   If NULL, uses `"center"` as default.
#' @param params Optional list of method-specific parameters for fine control.
#' @param .quiet If `TRUE`, suppress informational messages.
#'
#' @return A tibble with original data plus trend columns named `trend_{method}` or
#'   `trend_{method}_{suffix}` if suffix is provided. Rows come back in the
#'   order they were supplied in.
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom tibble as_tibble
#' @importFrom stats is.ts setNames
#' @importFrom vctrs vec_rbind
#'
#' @details
#' This function is designed for monthly (frequency = 12) and quarterly
#' (frequency = 4) economic data, and the defaults for each method follow the
#' conventions for those frequencies.
#'
#' For grouped data, the function applies trend extraction to each group separately,
#' maintaining the original data structure while adding trend columns.
#'
#' @examples
#' # Simple STL decomposition on quarterly GDP construction data
#' gdp_construction |> augment_trends(value_col = "index")
#'
#' # Multiple smoothing methods with unified parameter
#' gdp_construction |>
#'   augment_trends(
#'     value_col = "index",
#'     methods = c("hp", "loess", "ewma"),
#'     smoothing = 0.3
#'   )
#'
#' # Moving averages with unified window on monthly data
#' vehicles |>
#'   tail(60) |>
#'   augment_trends(
#'     value_col = "production",
#'     methods = c("ma", "wma", "triangular"),
#'     window = 8
#'   )
#'
#' # Economic indicators with different methods
#' ibcbr |>
#'   tail(48) |>
#'   augment_trends(
#'     value_col = "index",
#'     methods = c("median", "kalman", "kernel"),
#'     window = 9,
#'     smoothing = 0.15
#'   )
#'
#' # Moving average with right alignment (causal filter)
#' vehicles |>
#'   tail(60) |>
#'   augment_trends(
#'     value_col = "production",
#'     methods = "ma",
#'     window = 12,
#'     align = "right"
#'   )
#'
#' # Advanced: fine-tune specific methods
#' electric |>
#'   tail(72) |>
#'   augment_trends(
#'     value_col = "consumption",
#'     methods = "median",
#'     window = 7
#'   )
#'
#' # Multiple MA windows in a single call (adds trend_ma_3, trend_ma_6, trend_ma_12)
#' vehicles |>
#'   tail(60) |>
#'   augment_trends(
#'     value_col = "production",
#'     methods = "ma",
#'     window = c(3, 6, 12)
#'   )
#'
#' @export
augment_trends <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  group_vars = NULL,
  methods = "stl",
  frequency = NULL,
  suffix = NULL,
  window = NULL,
  smoothing = NULL,
  band = NULL,
  align = NULL,
  params = list(),
  .quiet = FALSE
) {
  group_cols <- .validate_augment_trends(
    data,
    date_col,
    value_col,
    group_cols,
    group_vars,
    methods,
    window,
    smoothing,
    band,
    align,
    params
  )
  if (
    !is.null(window) &&
      length(window) > 1 &&
      !any(methods %in% .WINDOW_VECTOR_METHODS)
  ) {
    cli::cli_warn(c(
      "Multiple {.arg window} values are only supported for {.val ma}, {.val median}, and {.val henderson} methods.",
      "i" = "Using first value ({window[1]}) for method(s) {.val {methods}}."
    ))
    window <- window[1]
  }
  data <- tibble::as_tibble(data)
  group_indices <- if (is.null(group_cols)) {
    list(seq_len(nrow(data)))
  } else {
    .index_group_indices(data, group_cols)
  }
  group_labels <- names(group_indices)
  frequency <- .resolve_augment_frequency(
    data,
    date_col,
    group_indices,
    frequency,
    .quiet
  )
  .inform_augment_frequency(methods, frequency, .quiet)

  if (!.quiet && length(group_indices) > 1) {
    cli::cli_inform(c(
      "Computing {length(methods)} method(s) for {length(group_indices)} group(s):",
      "i" = "Methods: {.val {methods}}",
      "i" = "Groups: {.val {group_labels}}"
    ))
  }

  computed <- vector("list", length(group_indices))
  name_map <- character()
  multiple_values <- length(value_col) > 1
  unit <- .frequency_unit(frequency)
  conditions <- .new_condition_log()
  # Replay on exit rather than after the loop, so a group that errors still
  # reports the warnings raised before it failed.
  on.exit(
    .replay_conditions(conditions, length(group_indices) > 1),
    add = TRUE
  )

  for (group_id in seq_along(group_indices)) {
    rows <- group_indices[[group_id]]
    group_data <- data[rows, , drop = FALSE]
    group_result <- tibble::tibble(.row_index = rows)
    data_key <- .period_key(group_data[[date_col]], unit)

    for (value_name in value_col) {
      value_suffix <- .augment_trend_suffix(
        value_name,
        multiple_values,
        suffix
      )
      # Each value column drops its own incomplete cases, so the periods a
      # trend covers are read back per column rather than once per group.
      trend_data <- .log_conditions(
        .compute_trend_columns(
          data = group_data,
          date_col = date_col,
          value_col = value_name,
          methods = methods,
          frequency = frequency,
          suffix = value_suffix,
          window = window,
          smoothing = smoothing,
          band = band,
          align = align,
          params = params,
          .quiet = .quiet
        ),
        group_labels[group_id],
        conditions,
        .quiet
      )
      if (is.null(trend_data)) {
        next
      }

      proposed_names <- setdiff(names(trend_data), date_col)
      for (proposed_name in proposed_names) {
        if (!proposed_name %in% names(name_map)) {
          final_name <- .unique_column_name(
            proposed_name,
            c(names(data), unname(name_map))
          )
          name_map[proposed_name] <- final_name
        }
      }

      idx <- match(data_key, .period_key(trend_data[[date_col]], unit))
      for (proposed_name in proposed_names) {
        group_result[[name_map[[proposed_name]]]] <-
          trend_data[[proposed_name]][idx]
      }
    }
    computed[[group_id]] <- group_result
  }

  computed <- vctrs::vec_rbind(!!!computed)
  computed <- computed[order(computed$.row_index), , drop = FALSE]
  result <- data
  trend_names <- setdiff(names(computed), ".row_index")
  for (trend_name in trend_names) {
    result[[trend_name]] <- computed[[trend_name]]
  }
  return(tibble::as_tibble(result))
}

#' Validate the front-end arguments of augment_trends()
#'
#' @description Runs once per public call and returns the group columns, which
#' the deprecated `group_vars` argument can still supply.
#' @noRd
.validate_augment_trends <- function(
  data,
  date_col,
  value_col,
  group_cols,
  group_vars,
  methods,
  window,
  smoothing,
  band,
  align,
  params
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

  .validate_methods(methods)

  if (!is.null(group_vars)) {
    cli::cli_warn(
      "{.arg group_vars} is deprecated. Use {.arg group_cols} instead."
    )
    if (is.null(group_cols)) group_cols <- group_vars
  }

  if (!is.null(group_cols)) {
    if (!is.character(group_cols)) {
      cli::cli_abort("{.arg group_cols} must be a character vector")
    }
    missing_group_cols <- setdiff(group_cols, names(data))
    if (length(missing_group_cols) > 0) {
      cli::cli_abort(
        "Group variables not found in data: {.val {missing_group_cols}}.
         Available columns: {.val {names(data)}}"
      )
    }
  }

  .validate_unified_params(window, smoothing, band, align, params)
  return(group_cols)
}

#' Settle the frequency used for every group in one call
#'
#' @description Detects from the first group when the caller supplied none,
#' matching the behaviour of the recursive implementation this replaced.
#' @noRd
.resolve_augment_frequency <- function(
  data,
  date_col,
  group_indices,
  frequency,
  .quiet
) {
  if (is.null(frequency)) {
    frequency <- .detect_frequency(
      data[[date_col]][group_indices[[1]]],
      .quiet = .quiet
    )
  }
  if (frequency < 1 || frequency > 365) {
    cli::cli_abort(
      "Frequency must be between 1 (annual) and 365 (daily), got {frequency}"
    )
  }

  return(frequency)
}

#' Suffix for one value column's generated names
#'
#' @description A single value column keeps the caller's suffix; several value
#' columns carry the column name so `trend_hp` becomes `trend_hp_sales`.
#' @noRd
.augment_trend_suffix <- function(value_col, multiple_values, suffix) {
  if (!multiple_values) {
    return(suffix)
  }
  if (is.null(suffix)) {
    return(value_col)
  }
  return(paste0(value_col, "_", suffix))
}

#' Announce a frequency the requested methods do not suit
#'
#' @description Only the STL fallback is announced here. The warning about
#' frequency-sensitive methods is left to `extract_trends()`, which raises it
#' for whichever series it actually sees; `.replay_conditions()` then reports it
#' once for the whole call.
#' @noRd
.inform_augment_frequency <- function(methods, frequency, .quiet) {
  if (.quiet) {
    return(invisible(NULL))
  }
  if ("stl" %in% methods && frequency == 1) {
    cli::cli_inform(
      "STL requires seasonal data (frequency > 1). Will use HP filter fallback for non-seasonal data."
    )
  }
  return(invisible(NULL))
}

#' Record the conditions raised while one group is computed
#'
#' @description Filter-level warnings name a fallback or an unreliable result,
#' so they have to reach the caller even though every group is computed behind
#' one consolidated message. Informational messages are dropped, and a warning
#' raised for several groups is recorded once against all of them.
#' @noRd
.new_condition_log <- function() {
  log <- new.env(parent = emptyenv())
  log$messages <- character()
  log$groups <- list()
  return(log)
}

#' @noRd
.log_conditions <- function(expr, group_label, log, .quiet) {
  if (.quiet) {
    return(expr)
  }

  return(withCallingHandlers(
    expr,
    message = function(cnd) {
      invokeRestart("muffleMessage")
    },
    warning = function(cnd) {
      text <- conditionMessage(cnd)
      position <- match(text, log$messages)
      if (is.na(position)) {
        log$messages <- c(log$messages, text)
        position <- length(log$messages)
        log$groups[[position]] <- character()
      }
      if (!is.null(group_label)) {
        log$groups[[position]] <- union(log$groups[[position]], group_label)
      }
      invokeRestart("muffleWarning")
    }
  ))
}

#' @noRd
.replay_conditions <- function(log, grouped) {
  for (position in seq_along(log$messages)) {
    text <- log$messages[position]
    groups <- log$groups[[position]]
    if (grouped && length(groups) > 0) {
      cli::cli_warn(c(
        "{text}",
        "i" = "{cli::qty(length(groups))}Affected group{?s}: {.val {groups}}"
      ))
    } else {
      cli::cli_warn("{text}")
    }
  }
  return(invisible(NULL))
}

#' Compute every generated column for one group and one value column
#'
#' @description Assumes validated input. Returns the dates the series was built
#' from alongside the generated columns, never the caller's data.
#' @noRd
.compute_trend_columns <- function(
  data,
  date_col,
  value_col,
  methods,
  frequency,
  suffix,
  window,
  smoothing,
  band,
  align,
  params,
  .quiet
) {
  ts_data <- .df_to_ts_internal(data, date_col, value_col, frequency)

  min_obs <- 3 * frequency
  if (length(ts_data) < min_obs) {
    cli::cli_warn(
      "Series has {length(ts_data)} observations.
       Minimum {min_obs} recommended for reliable trend extraction."
    )
  }

  trends <- extract_trends(
    ts_data = ts_data,
    methods = methods,
    window = window,
    smoothing = smoothing,
    band = band,
    align = align,
    params = params,
    .quiet = .quiet
  )

  time_base <- .time_base(ts_data)
  if (length(methods) == 1 && stats::is.ts(trends)) {
    trends_list <- setNames(list(trends), methods[1])
    trends_df <- .trends_to_df(
      trends_list,
      date_col,
      suffix,
      time_base = time_base
    )
    return(trends_df)
  }
  return(.trends_to_df(trends, date_col, suffix, time_base = time_base))
}

#' Normalise dates to the start of their period
#'
#' @description tsbox returns period-start dates while the caller's data can
#' hold end-of-month ones, so both sides of the match are floored first. A
#' frequency with no exact calendar period is matched on the date itself.
#' @noRd
.period_key <- function(dates, unit) {
  if (is.null(unit)) {
    return(dates)
  }
  return(lubridate::floor_date(dates, unit = unit))
}
