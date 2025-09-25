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
#' Converts a series, stored in a data.frame or tibble, into a ts object.
#'
#' @param x A `data.frame`, `tibble` or `data.table`.
#' @param date_colname Name of the date column. Defaults to `'date'`. Must be of class `Date`.
#' @param value_colname Name of the value column. Defaults to `'value'`. Must be `numeric`.
#' @param frequency The frequency of the series. Can be a shortened string (e.g. "M" for monthly) or a number (e.g. 12).
#'
#' @return A `ts` object
#' @export
#' @examples
#' ibc <- df_to_ts(ibcbr, value_colname = "ibcbr", frequency = "M")
#' class(ibc)
#' plot(ibc)
df_to_ts <- function(
    x,
    date_colname = "date",
    value_colname = "value",
    frequency = 12
    ) {

  # Check inputs

  # Check column names

  # Check if column names are present in data.frame
  nm <- names(x)
  if (!any(date_colname %in% nm) | !any(value_colname %in% nm)) {
    cli::cli_abort(
      "Column names {.val {c(date_colname, value_colname)}} not found in data.",
      "i" = "Available columns: {.val {names(x)}}"
    )
  }

  #> Select columns
  xvalue <- x[[value_colname]]
  xdate <- x[[date_colname]]

  if (any(is.na(xvalue))) {
    cli::cli_warn(
      "Missing values detected in {.val {value_colname}} column.",
      "i" = "Consider using interpolation methods to handle missing data."
    )
  }

  if (!inherits(xdate, "Date")) {
    cli::cli_abort(
      "Date column {.val {date_colname}} must be of type {.cls Date}, not {.cls {class(xdate)}}.",
      "i" = "Use {.code as.Date()} to convert your date column."
    )
  }

  if (any(is.na(xdate))) {
    cli::cli_warn(
      "Missing values detected in {.val {date_colname}} column.",
      "i" = "Using first non-NA date as starting point."
    )
  }

  xyear <- lubridate::year(min(xdate, na.rm = TRUE))
  xmonth <- lubridate::month(min(xdate, na.rm = TRUE))

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

  # Return time series
  y <- stats::ts(xvalue, start = c(xyear, xmonth), frequency = xfreq)

  return(y)

}
#> Avoid "no visible binding for global variable 'char'"
char <- NULL

#' Convert time series to tibble
#'
#' @param x A time series as a `ts` object
#' @param date_colname Optional name for the date column
#' @param value_colname Optional name for the value column
#'
#' @returns a `tibble`
#' @export
#' @examples
#' # example code
#' ts_to_df(AirPassengers)
#'
#' # Using a custom name for the value column
#' ts_to_df(AirPassengers, value_colname = "passengers")
ts_to_df <- function(x, date_colname = NULL, value_colname = NULL) {

  if (!stats::is.ts(x)) {
    cli::cli_abort(
      "Input {.arg x} must be a {.cls ts} object, not {.cls {class(x)}}.",
      "i" = "Use {.fn df_to_ts} to convert data.frame to ts object first."
    )
  }

  if (is.null(date_colname)) {
    date_colname <- "date"
  }

  if (is.null(value_colname)) {
    value_colname <- "value"
  }

  dat <- data.frame(
    d = zoo::as.Date.ts(x),
    v = zoo::coredata(x)
  )

  names(dat) <- c(date_colname, value_colname)
  dat <- tibble::as_tibble(dat)

  return(dat)

}

#' Internal data frame to time series conversion
#' @noRd
.df_to_ts_internal <- function(data, date_col, value_col, frequency) {
  # Extract date and value vectors
  dates <- data[[date_col]]
  values <- data[[value_col]]

  # Remove missing values
  complete_cases <- stats::complete.cases(dates, values)
  dates <- dates[complete_cases]
  values <- values[complete_cases]

  if (length(values) == 0) {
    cli::cli_abort("No complete cases found in data")
  }

  # Get start date components
  start_date <- min(dates)
  start_year <- lubridate::year(start_date)

  # Calculate start period based on frequency
  if (frequency == 12) {
    start_period <- lubridate::month(start_date)
  } else if (frequency == 4) {
    start_period <- lubridate::quarter(start_date)
  } else {
    start_period <- 1
  }

  # Create time series
  ts_obj <- stats::ts(
    values,
    start = c(start_year, start_period),
    frequency = frequency
  )

  return(ts_obj)
}

#' Convert trends list to data frame
#' @noRd
.trends_to_df <- function(trends, date_col, suffix) {
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
      next  # Skip invalid trends
    }

    # Convert to data frame
    trend_df <- data.frame(
      date = zoo::as.Date.ts(trend_ts),
      zoo::coredata(trend_ts)
    )

    # Create column name
    col_name <- if (is.null(suffix)) {
      paste0("trend_", method_name)
    } else {
      paste0("trend_", method_name, "_", suffix)
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
.safe_merge <- function(data, trends_df, date_col) {
  if (is.null(trends_df)) {
    return(data)
  }

  # Check for existing trend columns and create unique names
  existing_names <- names(data)
  new_names <- names(trends_df)[-1]  # Exclude date column

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
        "Column {.val {conflict}} already exists. Renamed trend column to {.val {new_name}}"
      )
    }
  }

  # Merge data frames
  result <- merge(data, trends_df, by = date_col, all.x = TRUE)

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
  } else if (common_diff >= 7 && common_diff <= 7) {
    52 # Weekly (exactly 7 days)
  } else if (common_diff >= 1 && common_diff <= 3) {
    252 # Daily (approximately 1-3 days, accounting for weekends)
  } else {
    # Try to infer from number of observations per year, but be more restrictive
    date_range <- as.numeric(max(dates) - min(dates))
    years_span <- date_range / 365.25
    obs_per_year <- length(dates) / years_span

    # Check if differences are too irregular (high variance)
    diff_variance <- stats::var(diffs)
    diff_mean <- mean(diffs)
    cv <- sqrt(diff_variance) / diff_mean # coefficient of variation

    # If coefficient of variation is too high, consider it irregular
    if (cv > 0.5) {
      cli::cli_abort(
        "Irregular time series detected (CV = {round(cv, 2)}).
         Please specify frequency manually."
      )
    }

    if (obs_per_year >= 10 && obs_per_year <= 14) {
      12 # Monthly
    } else if (obs_per_year >= 3 && obs_per_year <= 5) {
      4 # Quarterly
    } else {
      cli::cli_abort(
        "Cannot auto-detect frequency. Please specify manually.
         Detected {round(obs_per_year, 1)} observations per year"
      )
    }
  }

  if (!.quiet) {
    freq_name <- switch(
      as.character(frequency),
      "4" = "quarterly",
      "12" = "monthly",
      as.character(frequency)
    )
    cli::cli_inform("Auto-detected {freq_name} frequency ({frequency})")
  }

  return(frequency)
}

#' Validate economic frequency
#' @noRd
.validate_economic_frequency <- function(frequency) {
  if (!frequency %in% c(4, 12)) {
    cli::cli_abort(
      "Only monthly (12) and quarterly (4) frequencies are supported.
       Got frequency: {frequency}"
    )
  }
  return(TRUE)
}