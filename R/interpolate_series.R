#' Complete and interpolate a time series
#'
#' @description
#' Completes the calendar grid of one or more series and interpolates missing
#' values inside the observed span. Missing periods that are absent from the
#' input are made explicit before interpolation, so later observations remain
#' attached to the correct dates.
#'
#' Leading and trailing missing values are not extrapolated. By default, a
#' Kalman smoother is attempted first and linear interpolation is used if the
#' state-space model cannot be estimated.
#'
#' @param data A `data.frame`, `tibble`, or `data.table`.
#' @param date_col Name of the date column. It must have class `Date`.
#' @param value_col Name of one or more numeric value columns.
#' @param group_cols Optional character vector identifying separate series.
#' @param frequency Observations per year. Supported values are 1, 2, 4, 12,
#'   52, and 365. When `NULL`, the frequency is inferred from the dates.
#' @param method Interpolation method. One of `"auto"`, `"kalman"`,
#'   `"linear"`, or `"spline"`. `"auto"` tries a structural time-series
#'   Kalman smoother and falls back to linear interpolation if the model
#'   cannot be estimated; `"kalman"` errors instead. Both keep the Kalman
#'   estimates, with a warning, when the model fit does not converge.
#' @param max_gap Maximum consecutive missing periods to interpolate. `NULL`
#'   uses `floor(0.75 * n)`, where `n` is the span from the first through the
#'   last observed value. Use `Inf` to remove the limit.
#' @param imputed_col If `TRUE`, add one logical column per value column named
#'   `imputed_{value_col}`. A flag is true only where a value was filled.
#' @param .quiet If `TRUE`, suppress fallback, non-convergence,
#'   date-normalization, and high missingness warnings.
#'
#' @return A tibble on a complete date grid. Value columns are interpolated in
#'   place; leading and trailing missing values remain `NA`. Rows are ordered
#'   by group and date.
#'
#' @details
#' The Kalman method estimates a basic structural model with [stats::StructTS()]
#' when there are at least two observed seasonal cycles. Annual series and
#' seasonal series with fewer observations use a local linear trend model. Only
#' missing observations are replaced; observed values are never smoothed.
#'
#' Linear and cubic spline interpolation use [stats::approx()] and
#' [stats::spline()], respectively. Cubic splines can overshoot around sharp
#' changes, so linear interpolation is the fallback used by `method = "auto"`.
#'
#' Calendar conventions are detected separately within each group. Consistent
#' period starts, period ends, weekdays, and within-period calendar positions
#' are preserved. Inconsistent conventions are normalized to period starts.
#'
#' @examples
#' monthly <- data.frame(
#'   date = as.Date(c("2020-01-01", "2020-02-01", "2020-04-01")),
#'   value = c(1, 2, 4)
#' )
#'
#' interpolate_series(monthly, frequency = 12, method = "linear")
#' interpolate_series(
#'   monthly,
#'   frequency = 12,
#'   method = "linear",
#'   imputed_col = TRUE
#' )
#'
#' @export
interpolate_series <- function(
  data,
  date_col = "date",
  value_col = "value",
  group_cols = NULL,
  frequency = NULL,
  method = "auto",
  max_gap = NULL,
  imputed_col = FALSE,
  .quiet = FALSE
) {
  .validate_interpolation_input(
    data = data,
    date_col = date_col,
    value_col = value_col,
    group_cols = group_cols,
    frequency = frequency,
    method = method,
    max_gap = max_gap,
    imputed_col = imputed_col,
    .quiet = .quiet
  )

  data <- tibble::as_tibble(data)
  method <- match.arg(method, c("auto", "kalman", "linear", "spline"))

  if (is.null(group_cols)) {
    data_split <- list(data)
  } else {
    data_split <- .split_interpolation_groups(data, group_cols)
  }

  if (is.null(frequency)) {
    frequency <- .detect_frequency(data_split[[1]][[date_col]], .quiet = TRUE)
    if (frequency == 252) {
      frequency <- 365
      if (!.quiet) {
        cli::cli_warn(c(
          "Daily observations were interpreted as calendar-daily data.",
          "i" = "Business-day calendars are not supported by {.fn interpolate_series}."
        ))
      }
    } else if (!.quiet && frequency %in% c(1, 2, 4, 12, 52, 365)) {
      cli::cli_inform(
        "Auto-detected frequency {frequency} ({.val {.interpolation_unit(frequency)}})."
      )
    }
  }
  .validate_interpolation_frequency(frequency)

  results <- lapply(data_split, function(group_data) {
    result <- .complete_interpolation_grid(
      data = group_data,
      date_col = date_col,
      group_cols = group_cols,
      frequency = frequency,
      .quiet = .quiet
    )

    group_label <- .interpolation_group_label(group_data, group_cols)

    for (column in value_col) {
      filled <- .interpolate_vector(
        x = result[[column]],
        frequency = frequency,
        method = method,
        max_gap = max_gap,
        value_col = column,
        group_label = group_label,
        .quiet = .quiet
      )
      result[[column]] <- filled$value

      if (imputed_col) {
        result[[paste0("imputed_", column)]] <- filled$imputed
      }
    }

    result
  })

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  tibble::as_tibble(result)
}

.split_interpolation_groups <- function(data, group_cols) {
  keys <- unique(data[group_cols])
  key_order <- do.call(order, unname(keys))
  keys <- keys[key_order, , drop = FALSE]

  lapply(seq_len(nrow(keys)), function(index) {
    keep <- rep(TRUE, nrow(data))
    for (column in group_cols) {
      keep <- keep & data[[column]] == keys[[column]][index]
    }
    data[keep, , drop = FALSE]
  })
}

.validate_interpolation_input <- function(
  data,
  date_col,
  value_col,
  group_cols,
  frequency,
  method,
  max_gap,
  imputed_col,
  .quiet
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame, tibble, or data.table.")
  }
  .check_non_empty(data)

  if (!is.character(date_col) || length(date_col) != 1 || is.na(date_col)) {
    cli::cli_abort("{.arg date_col} must be a single column name.")
  }
  if (!is.character(value_col) || length(value_col) < 1 || anyNA(value_col)) {
    cli::cli_abort("{.arg value_col} must contain at least one column name.")
  }
  if (anyDuplicated(value_col)) {
    cli::cli_abort("{.arg value_col} must not contain duplicate names.")
  }

  required <- unique(c(date_col, value_col, group_cols))
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0) {
    cli::cli_abort("Column{?s} not found in data: {.val {missing_columns}}.")
  }

  if (!inherits(data[[date_col]], "Date")) {
    cli::cli_abort("Column {.val {date_col}} must be of class {.cls Date}.")
  }
  if (anyNA(data[[date_col]])) {
    cli::cli_abort("Column {.val {date_col}} must not contain missing values.")
  }

  non_numeric <- value_col[!vapply(data[value_col], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    cli::cli_abort("Column{?s} must be numeric: {.val {non_numeric}}.")
  }

  if (!is.null(group_cols)) {
    if (
      !is.character(group_cols) || length(group_cols) < 1 || anyNA(group_cols)
    ) {
      cli::cli_abort(
        "{.arg group_cols} must be a character vector of column names."
      )
    }
    if (anyDuplicated(group_cols)) {
      cli::cli_abort("{.arg group_cols} must not contain duplicate names.")
    }
    if (any(group_cols %in% c(date_col, value_col))) {
      cli::cli_abort(
        "{.arg group_cols} must be separate from date and value columns."
      )
    }
    if (anyNA(data[group_cols])) {
      cli::cli_abort("Grouping columns must not contain missing values.")
    }
  }

  if (
    !is.character(method) ||
      length(method) != 1 ||
      is.na(method) ||
      !method %in% c("auto", "kalman", "linear", "spline")
  ) {
    cli::cli_abort(
      "{.arg method} must be one of {.val auto}, {.val kalman}, {.val linear}, or {.val spline}."
    )
  }

  if (!is.null(frequency)) {
    .validate_interpolation_frequency(frequency)
  }

  if (!is.null(max_gap)) {
    valid_max_gap <- is.numeric(max_gap) &&
      length(max_gap) == 1 &&
      !is.na(max_gap) &&
      max_gap >= 0 &&
      (is.infinite(max_gap) || max_gap == floor(max_gap))
    if (!valid_max_gap) {
      cli::cli_abort(
        "{.arg max_gap} must be a non-negative integer, {.code Inf}, or {.code NULL}."
      )
    }
  }

  for (argument in c("imputed_col", ".quiet")) {
    value <- get(argument)
    if (!is.logical(value) || length(value) != 1 || is.na(value)) {
      cli::cli_abort(
        "{.arg {argument}} must be a single {.code TRUE} or {.code FALSE}."
      )
    }
  }

  if (imputed_col) {
    flag_columns <- paste0("imputed_", value_col)
    conflicts <- intersect(flag_columns, names(data))
    if (length(conflicts) > 0) {
      cli::cli_abort(
        "Imputation column name{?s} already present: {.val {conflicts}}."
      )
    }
  }

  invisible(NULL)
}

.validate_interpolation_frequency <- function(frequency) {
  supported <- c(1, 2, 4, 12, 52, 365)
  if (
    !is.numeric(frequency) ||
      length(frequency) != 1 ||
      is.na(frequency) ||
      !frequency %in% supported
  ) {
    cli::cli_abort(c(
      "{.arg frequency} must be one of {.val {supported}}.",
      "i" = "Business-day frequency 252 is not supported."
    ))
  }
  invisible(NULL)
}

.interpolation_unit <- function(frequency) {
  switch(
    as.character(frequency),
    "1" = "year",
    "2" = "6 months",
    "4" = "quarter",
    "12" = "month",
    "52" = "week",
    "365" = "day"
  )
}

.interpolation_periods <- function(dates, frequency) {
  unit <- .interpolation_unit(frequency)
  if (frequency == 365) {
    return(dates)
  }
  if (frequency == 52) {
    return(lubridate::floor_date(dates, unit = unit, week_start = 1))
  }
  lubridate::floor_date(dates, unit = unit)
}

.period_ends <- function(periods, unit) {
  lubridate::ceiling_date(periods, unit = unit) - lubridate::days(1)
}

.detect_date_convention <- function(dates, periods, frequency) {
  unit <- .interpolation_unit(frequency)

  if (all(dates == periods)) {
    return(list(type = "start"))
  }
  if (all(dates == .period_ends(periods, unit))) {
    return(list(type = "end"))
  }

  if (frequency == 52) {
    offsets <- as.integer(dates - periods)
    if (length(unique(offsets)) == 1) {
      return(list(type = "offset", days = offsets[1]))
    }
  } else if (frequency != 365) {
    month_offset <- lubridate::month(dates) - lubridate::month(periods)
    day <- lubridate::day(dates)
    if (length(unique(month_offset)) == 1 && length(unique(day)) == 1) {
      return(list(
        type = "calendar",
        months = month_offset[1],
        day = day[1]
      ))
    }
  }

  list(type = "start", normalized = TRUE)
}

.apply_date_convention <- function(periods, convention, frequency) {
  unit <- .interpolation_unit(frequency)
  switch(
    convention$type,
    start = periods,
    end = .period_ends(periods, unit),
    offset = periods + lubridate::days(convention$days),
    calendar = periods +
      lubridate::period(convention$months, units = "month") +
      lubridate::days(convention$day - 1)
  )
}

.complete_interpolation_grid <- function(
  data,
  date_col,
  group_cols,
  frequency,
  .quiet
) {
  periods <- .interpolation_periods(data[[date_col]], frequency)
  duplicated_periods <- sort(unique(periods[duplicated(periods)]))

  if (length(duplicated_periods) > 0) {
    shown <- .format_periods(duplicated_periods)
    more <- .more_periods(duplicated_periods)
    cli::cli_abort(c(
      "Found {length(duplicated_periods)} duplicated period{?s} in the data.",
      "x" = "Duplicated: {.val {shown}}{more}"
    ))
  }

  convention <- .detect_date_convention(data[[date_col]], periods, frequency)
  if (isTRUE(convention$normalized) && !.quiet) {
    cli::cli_warn(
      "Inconsistent date conventions were normalized to period starts."
    )
  }

  unit <- .interpolation_unit(frequency)
  expected <- seq(min(periods), max(periods), by = unit)
  positions <- match(expected, periods)
  result <- data[positions, , drop = FALSE]
  inserted <- is.na(positions)

  result[[date_col]] <- .apply_date_convention(expected, convention, frequency)
  if (!is.null(group_cols) && any(inserted)) {
    for (column in group_cols) {
      result[[column]][inserted] <- data[[column]][1]
    }
  }

  rownames(result) <- NULL
  tibble::as_tibble(result)
}

.interpolation_group_label <- function(data, group_cols) {
  if (is.null(group_cols)) {
    return("")
  }

  values <- vapply(
    group_cols,
    function(column) {
      paste0(column, "=", as.character(data[[column]][1]))
    },
    character(1)
  )
  paste0(" in group ", paste(values, collapse = ", "))
}

.longest_missing_run <- function(x) {
  runs <- rle(is.na(x))
  if (!any(runs$values)) {
    return(0L)
  }
  max(runs$lengths[runs$values])
}

.interpolate_vector <- function(
  x,
  frequency,
  method,
  max_gap,
  value_col,
  group_label,
  .quiet
) {
  observed <- which(!is.na(x))
  if (length(observed) < 2) {
    cli::cli_abort(
      "Column {.val {value_col}}{group_label} needs at least two observed values."
    )
  }

  span <- seq.int(observed[1], observed[length(observed)])
  missing <- span[is.na(x[span])]
  imputed <- rep(FALSE, length(x))

  if (length(missing) == 0) {
    return(list(value = x, imputed = imputed))
  }

  gap_limit <- if (is.null(max_gap)) floor(0.75 * length(span)) else max_gap
  longest_gap <- .longest_missing_run(x[span])
  if (longest_gap > gap_limit) {
    cli::cli_abort(c(
      "Missing gap is too long to interpolate in column {.val {value_col}}{group_label}.",
      "x" = "Longest gap: {longest_gap} periods; maximum allowed: {gap_limit}."
    ))
  }

  missing_share <- length(missing) / length(span)
  if (missing_share > 0.5 && !.quiet) {
    cli::cli_warn(
      "Interpolating {round(100 * missing_share)}% of the observed span in column {.val {value_col}}{group_label}."
    )
  }

  observed_in_span <- which(!is.na(x[span]))
  missing_in_span <- which(is.na(x[span]))
  y <- x[span]

  if (length(unique(y[observed_in_span])) == 1) {
    y[missing_in_span] <- y[observed_in_span[1]]
  } else if (method == "linear") {
    y <- .linear_interpolation(y)
  } else if (method == "spline") {
    y <- .spline_interpolation(y)
  } else {
    kalman <- tryCatch(
      .kalman_interpolation(y, frequency),
      error = function(error) error
    )

    if (inherits(kalman, "error")) {
      if (method == "kalman") {
        cli::cli_abort(c(
          "Kalman interpolation failed for column {.val {value_col}}{group_label}.",
          "x" = conditionMessage(kalman)
        ))
      }
      if (!.quiet) {
        cli::cli_warn(c(
          "Kalman interpolation failed for column {.val {value_col}}{group_label}; used linear interpolation.",
          "i" = conditionMessage(kalman)
        ))
      }
      y <- .linear_interpolation(y)
    } else {
      if (!isTRUE(attr(kalman, "converged")) && !.quiet) {
        cli::cli_warn(
          "Kalman model did not converge for column {.val {value_col}}{group_label}; estimates may be unreliable."
        )
      }
      y <- as.vector(kalman)
    }
  }

  x[span] <- y
  imputed[missing] <- !is.na(x[missing])
  list(value = x, imputed = imputed)
}

.linear_interpolation <- function(x) {
  observed <- which(!is.na(x))
  missing <- which(is.na(x))
  x[missing] <- stats::approx(
    x = observed,
    y = x[observed],
    xout = missing,
    rule = 1,
    ties = "ordered"
  )$y
  x
}

.spline_interpolation <- function(x) {
  observed <- which(!is.na(x))
  missing <- which(is.na(x))
  x[missing] <- stats::spline(
    x = observed,
    y = x[observed],
    xout = missing,
    method = "fmm",
    ties = "ordered"
  )$y
  x
}

.kalman_interpolation <- function(x, frequency) {
  ts_data <- stats::ts(x, frequency = frequency)
  n_observed <- sum(!is.na(x))
  type <- if (frequency > 1 && n_observed >= 2 * frequency) "BSM" else "trend"

  fit <- .fit_structts(ts_data, type)

  smooth <- stats::KalmanSmooth(ts_data, fit$model)$smooth
  estimates <- drop(smooth %*% fit$model$Z)
  missing <- is.na(x)

  if (length(estimates) != length(x) || any(!is.finite(estimates[missing]))) {
    stop("KalmanSmooth returned invalid estimates.", call. = FALSE)
  }

  x[missing] <- estimates[missing]
  attr(x, "converged") <- fit$code == 0
  x
}

.fit_structts <- function(ts_data, type) {
  suppressWarnings(stats::StructTS(ts_data, type = type))
}
