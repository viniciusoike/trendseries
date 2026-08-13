#' Data Format Conversion Utilities
#'
#' @description Functions for converting between different time series formats,
#' frequency detection, and data frame manipulation for the trendseries package.
#' These functions handle the interface between tibble/data.frame workflows and
#' time series objects.
#'
#' @name converters
NULL

#' Convert a data.frame into a time series (ts)
#'
#' @description
#' Converts a series, stored in a `data.frame` or `tibble`, into a `ts` object.
#'
#' A `ts` positions observations by period rather than by date, so the input
#' must cover a complete run of periods. Rows are sorted before conversion, and
#' a missing or duplicated period is rejected rather than silently shifting
#' every later observation onto the wrong date. Missing *values* are kept in
#' place, which leaves the series correctly dated.
#'
#' @param x A `data.frame`, `tibble` or `data.table`.
#' @param date_col Name of the date column. Defaults to `'date'`. Must be of class `Date`.
#' @param value_col Name of the value column. Defaults to `'value'`. Must be `numeric`.
#' @param frequency The frequency of the series. Can be a shortened string (e.g. "M" for monthly) or a number (e.g. 12).
#'
#' @return A `ts` object
#' @export
#' @examples
#' ibc <- df_to_ts(ibcbr, value_col = "index", frequency = "M")
#' class(ibc)
#' plot(ibc)
df_to_ts <- function(
  x,
  date_col = "date",
  value_col = "value",
  frequency = 12
) {
  # Check inputs

  # Check if column names are present in data.frame
  nm <- names(x)
  if (!any(date_col %in% nm) | !any(value_col %in% nm)) {
    cli::cli_abort(
      "Column names {.val {c(date_col, value_col)}} not found in data."
    )
  }

  # Select columns
  xvalue <- x[[value_col]]
  xdate <- x[[date_col]]

  if (any(is.na(xvalue))) {
    cli::cli_warn(
      "Missing values detected in {.val {value_col}} column.",
      "i" = "They are kept in place, so the series stays correctly dated."
    )
  }

  if (!inherits(xdate, "Date")) {
    cli::cli_abort(
      "Date column {.val {date_col}} must be of type {.cls Date}, not {.cls {class(xdate)}}.",
      "i" = "Use {.code as.Date()} to convert your date column."
    )
  }

  #> Check frequency argument
  available_freqs <- data.frame(
    char = c("A", "S", "Q", "M", "W", "D"),
    num = c(1, 2, 4, 12, 52, 252)
  )

  if (is.character(frequency)) {
    if (!any(frequency %in% available_freqs$char)) {
      cli::cli_abort(
        "Frequency {.val {frequency}} not recognized.",
        "i" = "Supported frequencies: {.val {available_freqs$char}}"
      )
    }

    xfreq <- subset(available_freqs, char == frequency)$num
  }

  if (is.numeric(frequency)) {
    if (!any(frequency %in% available_freqs$num)) {
      cli::cli_abort(
        "Frequency {.val {frequency}} not supported.",
        "i" = "Supported frequencies: {.val {available_freqs$num}}"
      )
    }

    xfreq <- frequency
  }

  # A row with no date cannot be positioned, and leaving it in place would push
  # every later observation onto the wrong period
  has_date <- !is.na(xdate)
  if (!all(has_date)) {
    cli::cli_warn(
      "Dropped {sum(!has_date)} row{?s} with a missing {.val {date_col}} value."
    )
  }
  xvalue <- xvalue[has_date]
  xdate <- xdate[has_date]

  if (length(xdate) == 0) {
    cli::cli_abort("No dated observations found in data.")
  }

  # Observations are positioned by period, not by date, so the rows have to be
  # ordered and the grid has to be complete before any of this means anything
  ord <- order(xdate)
  xvalue <- xvalue[ord]
  xdate <- xdate[ord]

  .check_regular_grid(xdate, xfreq)

  # Return time series
  y <- stats::ts(
    xvalue,
    start = c(lubridate::year(xdate[1]), .start_period(xdate[1], xfreq)),
    frequency = xfreq
  )

  return(y)
}
#> Avoid "no visible binding for global variable 'char'"
char <- NULL

#' Convert time series to tibble
#'
#' @description Converts a `ts` object into a `tibble` with two columns: `date`
#' and `value`. Optionally, the columns can be renammed using `date_col` and
#' `value_col`.
#'
#' @param x A time series as a `ts` object
#' @param date_col Name for the date column. Defaults to `"date"``.
#' @param value_col Name for the value column. Defaults to `"value"`.
#'
#' @returns a `tibble`
#' @export
#' @examples
#' # example code
#' ts_to_df(AirPassengers)
#'
#' # Using a custom name for the value column
#' ts_to_df(AirPassengers, value_col = "passengers")
ts_to_df <- function(x, date_col = NULL, value_col = NULL) {
  if (!stats::is.ts(x)) {
    cli::cli_abort(
      "Input {.arg x} must be a {.cls ts} object, not {.cls {class(x)}}."
    )
  }

  if (is.null(date_col)) {
    date_col <- "date"
  }

  if (is.null(value_col)) {
    value_col <- "value"
  }

  # Use tsbox for conversion
  dat <- tsbox::ts_df(x)

  # Rename columns to match user preference
  names(dat) <- c(date_col, value_col)
  dat <- tibble::as_tibble(dat)

  return(dat)
}

#' Period unit corresponding to a series frequency
#'
#' @description Returns the `seq.Date()` step for the frequencies where a
#' calendar period is exactly defined, or `NULL` otherwise. Weekly and daily
#' series have no exact calendar period, so grid checks are skipped for them.
#' @noRd
.frequency_unit <- function(frequency) {
  unit <- switch(
    as.character(frequency),
    "12" = "month",
    "4" = "quarter",
    "2" = "6 months",
    "1" = "year",
    NULL
  )
  return(unit)
}

#' Position of a date within its year, in units of the series frequency
#'
#' @description `ts()` reads `start = c(year, period)` in units of `frequency`,
#' so the period has to be counted in those units. Taking the calendar month
#' regardless of frequency puts a quarterly series starting in April at Q4 and
#' an annual series dated March at year + 2.
#'
#' Weekly and daily series have no exact calendar period. Their start is placed
#' proportionally within the year, which is approximate but keeps a series
#' beginning in July from claiming to begin in January.
#' @noRd
.start_period <- function(date, frequency) {
  period <- switch(
    as.character(frequency),
    "1" = 1,
    "2" = lubridate::semester(date),
    "4" = lubridate::quarter(date),
    "12" = lubridate::month(date),
    floor(lubridate::yday(date) / 366 * frequency) + 1
  )

  return(as.integer(period))
}

#' Format a handful of periods for an error message
#'
#' @description Truncates to the first five so long gap lists stay readable.
#' @noRd
.format_periods <- function(periods, n = 5L) {
  return(format(periods[seq_len(min(n, length(periods)))]))
}

#' Trailing count note for a truncated period list
#' @noRd
.more_periods <- function(periods, n = 5L) {
  if (length(periods) <= n) {
    return("")
  }
  return(paste0(" (+", length(periods) - n, " more)"))
}

#' Require a complete, non-duplicated grid of periods
#'
#' @description Checks if the series is properly formatted, meaning it has
#' no duplicate periods and no missing values.
#'
#' @param dates Dates that will become observations, after any filtering.
#' @param frequency Series frequency.
#' @param dropped Dates excluded because their value was missing, used to
#'   explain which gaps the caller introduced.
#' @noRd
.check_regular_grid <- function(dates, frequency, dropped = NULL) {
  unit <- .frequency_unit(frequency)
  if (is.null(unit) || length(dates) < 2) {
    return(invisible(NULL))
  }

  # Normalise to period start so end-of-month conventions compare equal
  periods <- lubridate::floor_date(dates, unit = unit)

  duplicated_periods <- sort(unique(periods[duplicated(periods)]))
  if (length(duplicated_periods) > 0) {
    shown <- .format_periods(duplicated_periods)
    more <- .more_periods(duplicated_periods)
    cli::cli_abort(c(
      "Found {length(duplicated_periods)} duplicated period{?s} in the data.",
      "x" = "Duplicated: {.val {shown}}{more}"
    ))
  }

  observed <- sort(periods)
  expected <- seq(observed[1], observed[length(observed)], by = unit)
  missing <- expected[!expected %in% observed]

  if (length(missing) == 0) {
    return(invisible(NULL))
  }

  # Separate gaps the caller created by dropping NA values from gaps that were
  # already absent, so the message points at the right fix
  from_na <- if (is.null(dropped) || length(dropped) == 0) {
    missing[0]
  } else {
    missing[missing %in% lubridate::floor_date(dropped, unit = unit)]
  }
  absent <- missing[!missing %in% from_na]

  detail <- character(0)
  if (length(from_na) > 0) {
    detail <- c(
      detail,
      "*" = "{length(from_na)} period{?s} had a missing value."
    )
  }
  if (length(absent) > 0) {
    detail <- c(
      detail,
      "*" = "{length(absent)} period{?s} {?is/are} absent from the data."
    )
  }

  shown <- .format_periods(missing)
  more <- .more_periods(missing)
  cli::cli_abort(c(
    "Series has {length(missing)} missing period{?s}.",
    detail,
    "x" = "Missing: {.val {shown}}{more}"
  ))
}

#' Internal data frame to time series conversion
#' @noRd
.df_to_ts_internal <- function(data, date_col, value_col, frequency) {
  # Extract date and value vectors
  dates <- data[[date_col]]
  values <- data[[value_col]]

  # Remove missing values
  complete_cases <- stats::complete.cases(dates, values)
  dropped_dates <- dates[!complete_cases & !is.na(dates)]
  dates <- dates[complete_cases]
  values <- values[complete_cases]

  if (length(values) == 0) {
    cli::cli_abort("No complete cases found in data")
  }

  # Sort by date to ensure correct time series ordering
  ord <- order(dates)
  dates <- dates[ord]
  values <- values[ord]

  # Dropping incomplete cases can open interior gaps; leading and trailing
  # missing values are harmless because `start` is taken from the retained rows
  .check_regular_grid(dates, frequency, dropped = dropped_dates)

  # Get start date components
  start_date <- min(dates)

  # Create time series
  ts_obj <- stats::ts(
    values,
    start = c(
      lubridate::year(start_date),
      .start_period(start_date, frequency)
    ),
    frequency = frequency
  )

  return(ts_obj)
}

#' Internal data frame to time series conversion, preserving missing values
#'
#' @description Like `.df_to_ts_internal()`, but rows with a missing value are
#' kept rather than dropped. Dropping them shortens the series and shifts every
#' later observation back one period, because the `ts` is built assuming
#' contiguous periods from `start`. Rolling aggregations are position-based, so
#' they need the gaps left in place; `na_rm` then controls how each window
#' treats them. Rows with a missing date cannot be positioned and are dropped.
#' @noRd
.df_to_ts_preserve_na <- function(data, date_col, value_col, frequency) {
  dates <- data[[date_col]]
  values <- data[[value_col]]

  has_date <- !is.na(dates)
  if (!all(has_date)) {
    cli::cli_warn(
      "Dropped {sum(!has_date)} row{?s} with a missing {.val {date_col}} value"
    )
  }
  dates <- dates[has_date]
  values <- values[has_date]

  if (length(values) == 0) {
    cli::cli_abort("No dated observations found in data")
  }

  ord <- order(dates)
  dates <- dates[ord]
  values <- values[ord]

  # Rows with a missing value are kept, so only genuinely absent periods can
  # break the position-to-date correspondence
  .check_regular_grid(dates, frequency)

  start_date <- dates[1]

  ts_obj <- stats::ts(
    values,
    start = c(
      lubridate::year(start_date),
      .start_period(start_date, frequency)
    ),
    frequency = frequency
  )

  return(ts_obj)
}

#' Periods at given positions of a series, as Dates
#'
#' @description Turns `time()` values back into calendar dates so gap messages
#' read the same whether they came from the data.frame path or from a `ts`.
#' Frequencies with no exact calendar period fall back to the numeric time.
#' @noRd
.ts_periods <- function(ts_data, index) {
  times <- as.numeric(stats::time(ts_data))[index]
  freq <- stats::frequency(ts_data)

  if (is.null(.frequency_unit(freq))) {
    return(format(times))
  }

  # Nudge before flooring: December of a monthly series can be stored as
  # 2014.99999... rather than 2014.91666...
  year <- floor(times + 1e-6)
  step <- pmin(round((times - year) * freq), freq - 1)
  month <- round(step * 12 / freq) + 1

  return(as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month))))
}

#' Locate the observed span of a series, rejecting interior gaps
#'
#' @description Unlike the data.frame path, a `ts` already fixes the
#' position-to-date correspondence, so a missing value cannot misdate anything.
#' What it does instead is reach the filters, where the behaviour is
#' method-dependent and mostly silent: some methods error, some return an
#' all-`NA` series, and the recursive ones (`ewma`, `bn`) propagate the gap to
#' every later observation. Rejecting is the only outcome a caller can predict.
#'
#' Leading and trailing missing values are a different case. They only shorten
#' the estimation window, which is what `.df_to_ts_internal()` already does by
#' dropping those rows, so they are trimmed rather than rejected.
#'
#' @param ts_data A `ts` object.
#' @returns The first and last observed positions.
#' @noRd
.observed_span <- function(ts_data) {
  observed <- which(!is.na(as.numeric(ts_data)))

  if (length(observed) == 0) {
    cli::cli_abort("Series has no non-missing values.")
  }

  first <- observed[1]
  last <- observed[length(observed)]
  interior <- setdiff(seq.int(first, last), observed)

  if (length(interior) > 0) {
    periods <- .ts_periods(ts_data, interior)
    shown <- .format_periods(periods)
    more <- .more_periods(periods)
    cli::cli_abort(c(
      "Series has {length(interior)} missing value{?s} inside the observed range.",
      "x" = "Missing: {.val {shown}}{more}",
      "i" = "Impute the {cli::qty(length(interior))}gap{?s} before extracting a trend."
    ))
  }

  return(c(first, last))
}

#' Restrict a series to its observed span
#'
#' @description Returns `NULL` when nothing needs trimming, so the common case
#' of a complete series carries no padding step afterwards.
#' @noRd
.trim_to_observed <- function(ts_data, span) {
  if (span[1] == 1 && span[2] == length(ts_data)) {
    return(NULL)
  }

  trimmed <- stats::ts(
    as.numeric(ts_data)[seq.int(span[1], span[2])],
    start = as.numeric(stats::time(ts_data))[span[1]],
    frequency = stats::frequency(ts_data)
  )

  return(trimmed)
}

#' Put a result back on the input's time base
#'
#' @description Trimming leading and trailing missing values shortens the
#' series, but callers expect a result aligned with what they passed in. The
#' trimmed-away periods come back as `NA`, which is what a trend is there.
#' Matching on time rather than position keeps this correct for any method that
#' returns a different span than it was given.
#' @noRd
.restore_time_base <- function(trends, template) {
  if (is.null(template)) {
    return(trends)
  }

  if (stats::is.ts(trends)) {
    return(.pad_to_template(trends, template))
  }

  return(lapply(trends, .pad_to_template, template = template))
}

#' @noRd
.pad_to_template <- function(trend, template) {
  if (is.null(trend) || !stats::is.ts(trend)) {
    return(trend)
  }

  out <- template
  out[] <- NA_real_

  index <- match(
    round(as.numeric(stats::time(trend)), 6),
    round(as.numeric(stats::time(out)), 6)
  )
  keep <- !is.na(index)
  out[index[keep]] <- as.numeric(trend)[keep]

  return(out)
}

#' Convert trends list to data frame
#'
#' @description `prefix` lets other families reuse this (e.g. `augment_rolling()`
#' passes `"roll_"`), so generated column names stay consistent across the package.
#' @noRd
.trends_to_df <- function(trends, date_col, suffix, prefix = "trend_") {
  if (is.null(trends) || length(trends) == 0) {
    return(NULL)
  }

  # Handle single ts object (convert to list first)
  if (stats::is.ts(trends)) {
    trends <- list(trend = trends)
  }

  # Convert each trend to data frame
  trend_dfs <- list()

  for (method_name in names(trends)) {
    trend_ts <- trends[[method_name]]
    if (is.null(trend_ts) || !stats::is.ts(trend_ts)) {
      next # Skip invalid trends
    }

    # Convert to data frame using tsbox
    trend_df <- tsbox::ts_df(trend_ts)

    # Create column name
    col_name <- if (is.null(suffix)) {
      paste0(prefix, method_name)
    } else {
      paste0(prefix, method_name, "_", suffix)
    }

    names(trend_df) <- c(date_col, col_name)
    trend_dfs[[method_name]] <- trend_df
  }

  if (length(trend_dfs) == 0) {
    return(NULL)
  }

  # Merge all trend data frames
  result <- trend_dfs[[1]]
  if (length(trend_dfs) > 1) {
    for (i in 2:length(trend_dfs)) {
      result <- merge(result, trend_dfs[[i]], by = date_col, all = TRUE)
    }
  }

  return(tibble::as_tibble(result))
}

#' Safely merge data with trends, handling naming conflicts
#' @noRd
.safe_merge <- function(data, trends_df, date_col, frequency = NULL) {
  if (is.null(trends_df)) {
    return(data)
  }

  # Check for existing trend columns and create unique names
  existing_names <- names(data)
  new_names <- names(trends_df)[-1] # Exclude date column

  # Find conflicts and resolve them
  conflicts <- intersect(existing_names, new_names)
  if (length(conflicts) > 0) {
    for (conflict in conflicts) {
      # Find a unique name
      counter <- 1
      new_name <- paste0(conflict, "_", counter)
      while (new_name %in% existing_names) {
        counter <- counter + 1
        new_name <- paste0(conflict, "_", counter)
      }

      # Rename in trends_df
      names(trends_df)[names(trends_df) == conflict] <- new_name

      cli::cli_warn(
        "Column {.val {conflict}} already exists. Renamed new column to {.val {new_name}}"
      )
    }
  }

  # Normalize date columns to period-start for robust joining.
  # tsbox::ts_df() always produces first-of-period dates, but the original
  # data may use end-of-month or other conventions.
  if (!is.null(frequency)) {
    unit <- if (frequency == 12) {
      "month"
    } else if (frequency == 4) {
      "quarter"
    } else {
      "year"
    }
    data$.join_key <- lubridate::floor_date(data[[date_col]], unit = unit)
    trends_df$.join_key <- lubridate::floor_date(
      trends_df[[date_col]],
      unit = unit
    )

    trend_cols <- setdiff(names(trends_df), date_col)
    result <- merge(
      data,
      trends_df[, trend_cols, drop = FALSE],
      by = ".join_key",
      all.x = TRUE
    )
    result$.join_key <- NULL
  } else {
    result <- merge(data, trends_df, by = date_col, all.x = TRUE)
  }

  # Ensure we return a tibble
  result <- tibble::as_tibble(result)

  return(result)
}

#' Detect frequency from date vector
#' @noRd
.detect_frequency <- function(dates, .quiet = FALSE) {
  if (length(dates) < 2) {
    cli::cli_abort("Need at least 2 observations to detect frequency")
  }

  # Calculate differences between consecutive dates
  diffs <- as.numeric(diff(sort(dates)))

  # Remove zero differences (duplicates)
  diffs <- diffs[diffs > 0]

  if (length(diffs) == 0) {
    cli::cli_abort("No time variation found in dates")
  }

  # Find most common difference (mode)
  common_diff <- as.numeric(names(sort(table(diffs), decreasing = TRUE))[1])

  # Map to frequency based on common difference
  frequency <- if (common_diff >= 28 && common_diff <= 32) {
    12 # Monthly (approximately 30 days)
  } else if (common_diff >= 85 && common_diff <= 95) {
    4 # Quarterly (approximately 90 days)
  } else if (common_diff >= 175 && common_diff <= 190) {
    2 # Semi-annual (approximately 180 days)
  } else if (common_diff >= 360 && common_diff <= 370) {
    1 # Annual (approximately 365 days)
  } else if (common_diff >= 6 && common_diff <= 8) {
    52 # Weekly (approximately 7 days)
  } else if (common_diff >= 1 && common_diff <= 3) {
    252 # Daily (approximately 1-3 days, accounting for weekends)
  } else {
    # Try to infer from number of observations per year
    date_range <- as.numeric(max(dates) - min(dates))
    years_span <- date_range / 365.25
    obs_per_year <- length(dates) / years_span

    # Check if differences are too irregular (high variance)
    diff_variance <- stats::var(diffs)
    diff_mean <- mean(diffs)
    cv <- sqrt(diff_variance) / diff_mean # coefficient of variation

    # If coefficient of variation is too high, warn but try to estimate
    if (cv > 0.5) {
      if (!.quiet) {
        cli::cli_warn(
          "Irregular time series detected (CV = {round(cv, 2)}).
           Auto-detected frequency may be inaccurate. Consider specifying frequency manually."
        )
      }
    }

    # More flexible frequency detection
    if (obs_per_year >= 0.8 && obs_per_year <= 1.2) {
      1 # Annual
    } else if (obs_per_year >= 1.8 && obs_per_year <= 2.2) {
      2 # Semi-annual
    } else if (obs_per_year >= 3 && obs_per_year <= 5) {
      4 # Quarterly
    } else if (obs_per_year >= 10 && obs_per_year <= 14) {
      12 # Monthly
    } else if (obs_per_year >= 48 && obs_per_year <= 56) {
      52 # Weekly
    } else if (obs_per_year >= 240 && obs_per_year <= 260) {
      252 # Daily (trading days)
    } else {
      # Best guess: round to nearest standard frequency
      rounded <- round(obs_per_year)
      if (!.quiet) {
        cli::cli_warn(
          "Non-standard frequency detected: {round(obs_per_year, 1)} observations per year.
           Using frequency = {rounded}. Consider specifying frequency manually if this is incorrect."
        )
      }
      rounded
    }
  }

  if (!.quiet) {
    freq_name <- switch(
      as.character(frequency),
      "1" = "annual",
      "2" = "semi-annual",
      "4" = "quarterly",
      "12" = "monthly",
      "52" = "weekly",
      "252" = "daily (trading days)",
      "365" = "daily",
      paste0("frequency ", frequency)
    )
    cli::cli_inform("Auto-detected {freq_name} ({frequency} obs/year)")
  }

  return(frequency)
}

#' Validate economic frequency
#' @noRd
.validate_economic_frequency <- function(frequency) {
  if (!frequency %in% c(4, 12)) {
    cli::cli_abort(
      "Only monthly (12) and quarterly (4) frequencies are supported."
    )
  }
  return(TRUE)
}
