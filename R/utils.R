#' Auto-detect frequency from date column
#' @noRd
.detect_frequency <- function(dates, .quiet = FALSE) {
  if (!inherits(dates, "Date")) {
    cli::cli_abort("Date column must be of class Date")
  }

  # Remove missing dates
  dates <- dates[!is.na(dates)]

  if (length(dates) < 2) {
    cli::cli_abort("Need at least 2 non-missing dates to detect frequency")
  }

  # Sort dates
  dates <- sort(dates)

  # Calculate differences
  diffs <- as.numeric(diff(dates))

  # Get most common difference
  common_diff <- as.numeric(names(sort(table(diffs), decreasing = TRUE))[1])

  # Map to frequency based on common difference
  frequency <- if (common_diff >= 28 && common_diff <= 32) {
    12  # Monthly (approximately 30 days)
  } else if (common_diff >= 85 && common_diff <= 95) {
    4   # Quarterly (approximately 90 days)
  } else if (common_diff >= 7 && common_diff <= 7) {
    52  # Weekly (exactly 7 days)
  } else if (common_diff >= 1 && common_diff <= 3) {
    252  # Daily (approximately 1-3 days, accounting for weekends)
  } else {
    # Try to infer from number of observations per year, but be more restrictive
    date_range <- as.numeric(max(dates) - min(dates))
    n_obs <- length(dates)
    obs_per_year <- n_obs / (date_range / 365.25)

    # Check if differences are too irregular (high variance)
    diff_variance <- stats::var(diffs)
    diff_mean <- mean(diffs)
    cv <- sqrt(diff_variance) / diff_mean  # coefficient of variation

    # If coefficient of variation is too high, consider it irregular
    if (cv > 0.5) {
      cli::cli_abort(
        "Cannot auto-detect frequency. Irregular time intervals detected.
         Common difference: {common_diff} days, observations per year: {round(obs_per_year, 1)}"
      )
    }

    if (obs_per_year >= 10 && obs_per_year <= 14) {
      12  # Monthly
    } else if (obs_per_year >= 3 && obs_per_year <= 5) {
      4   # Quarterly
    } else {
      cli::cli_abort(
        "Cannot auto-detect frequency. Please specify manually.
         Common difference: {common_diff} days, observations per year: {round(obs_per_year, 1)}"
      )
    }
  }

  if (!.quiet) {
    freq_name <- switch(as.character(frequency),
      "4" = "quarterly",
      "12" = "monthly",
      as.character(frequency)
    )
    cli::cli_inform("Auto-detected frequency: {freq_name} ({frequency})")
  }

  return(frequency)
}

#' Internal function to convert data frame to ts
#' @noRd
.df_to_ts_internal <- function(data, date_col, value_col, frequency) {
  dates <- data[[date_col]]
  values <- data[[value_col]]

  # Remove missing values
  complete_cases <- !is.na(dates) & !is.na(values)
  dates <- dates[complete_cases]
  values <- values[complete_cases]

  if (length(values) == 0) {
    cli::cli_abort("No complete cases found (non-missing date and value)")
  }

  # Sort by date
  order_idx <- order(dates)
  dates <- dates[order_idx]
  values <- values[order_idx]

  # Determine start year and period
  start_date <- min(dates)
  start_year <- lubridate::year(start_date)

  if (frequency == 12) {
    start_period <- lubridate::month(start_date)
  } else if (frequency == 4) {
    start_period <- lubridate::quarter(start_date)
  } else {
    start_period <- 1
  }

  # Create time series
  ts_obj <- stats::ts(values, start = c(start_year, start_period), frequency = frequency)

  return(ts_obj)
}

#' Convert trends list to data frame
#' @noRd
.trends_to_df <- function(trends, date_col, suffix) {
  if (is.null(trends)) {
    return(NULL)
  }

  # Handle single trend vs multiple trends
  if (stats::is.ts(trends)) {
    # Single trend - use generic "trend" name for direct ts input
    trends_list <- list(trend = trends)
  } else {
    # Multiple trends (or single trend passed as list)
    trends_list <- trends
  }

  # Convert each trend to data frame
  trend_dfs <- lapply(names(trends_list), function(method) {
    trend_ts <- trends_list[[method]]
    if (is.null(trend_ts) || all(is.na(trend_ts))) {
      return(NULL)
    }

    # Convert to data frame
    df <- data.frame(
      date = zoo::as.Date.ts(trend_ts),
      value = as.numeric(trend_ts)
    )

    # Create column name
    if (method == "trend") {
      # Direct ts input - use simple "trend" name
      col_name <- "trend"
    } else if (is.null(suffix)) {
      col_name <- paste0("trend_", method)
    } else {
      col_name <- paste0("trend_", method, "_", suffix)
    }

    names(df)[2] <- col_name
    names(df)[1] <- date_col

    return(df)
  })

  # Remove NULL entries
  trend_dfs <- trend_dfs[!sapply(trend_dfs, is.null)]

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

#' Safe merge that handles naming conflicts
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
    # Create mapping for new names
    name_mapping <- new_names
    names(name_mapping) <- new_names

    for (conflict in conflicts) {
      # Find a unique name
      counter <- 1
      new_name <- paste0(conflict, "_", counter)
      while (new_name %in% existing_names || new_name %in% new_names) {
        counter <- counter + 1
        new_name <- paste0(conflict, "_", counter)
      }
      name_mapping[conflict] <- new_name

      # Update trends_df column name
      col_idx <- which(names(trends_df) == conflict)
      names(trends_df)[col_idx] <- new_name
    }

    if (length(conflicts) > 0) {
      cli::cli_inform(
        "Renamed conflicting columns: {.val {conflicts}} -> {.val {name_mapping[conflicts]}}"
      )
    }
  }

  # Perform merge
  result <- merge(data, trends_df, by = date_col, all.x = TRUE)

  return(tibble::as_tibble(result))
}

#' Validate economic frequency
#' @noRd
.validate_economic_frequency <- function(frequency) {
  if (!frequency %in% c(4, 12)) {
    cli::cli_abort(
      "Only monthly (12) and quarterly (4) frequencies are supported for economic data.
       Provided frequency: {frequency}"
    )
  }
  return(TRUE)
}