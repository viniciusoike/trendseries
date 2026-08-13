#' Rolling aggregations for time series objects
#'
#' @description
#' Compute rolling and year-to-date aggregations of a time series. Unlike
#' [extract_trends()], which estimates a trend in the units of the series, these
#' are *aggregations*: a 12-month rolling sum is a 12-month total, not a level
#' estimate. The two families are kept separate for that reason, so rolling
#' results are not accepted by [detrend_series()].
#'
#' @param ts_data A time series object (`ts`, `xts`, or `zoo`) or any object
#'   convertible via tsbox.
#' @param stats Character vector of rolling statistics. Options: `"sum"`
#'   (rolling total of flows), `"chain"` (compound accumulation of rates,
#'   `prod(1 + r) - 1`), `"mean"`, `"sd"`, `"min"`, `"max"`. Default is
#'   `"sum"`.
#' @param window Window length in periods. If `NULL`, defaults to the series
#'   frequency (12 for monthly, 4 for quarterly). A numeric vector runs the
#'   statistic once per window value. Alternatively, the string `"ytd"`
#'   computes an expanding year-to-date accumulation that resets each January
#'   (or Q1). Numeric windows and `"ytd"` cannot be mixed in one call.
#' @param align Alignment of the window relative to the output position:
#'   `"right"` (default, causal — uses the current and preceding observations),
#'   `"center"`, or `"left"`. Right alignment is the convention for accumulated
#'   economic indicators. Ignored when `window = "ytd"`.
#' @param percent Only used by `stats = "chain"`. If `FALSE` (default), rates
#'   are assumed to be decimals (0.005 for 0.5%). If `TRUE`, rates are assumed
#'   to be percentages (0.5 for 0.5%) and the result is returned in percent.
#' @param na_rm If `TRUE`, missing values are ignored within each window. The
#'   default `FALSE` propagates `NA`, so an incomplete window yields `NA`.
#' @param .quiet If `TRUE`, suppress informational messages.
#'
#' @return If a single statistic and a single window are requested, a `ts`
#'   object. Otherwise a named list of `ts` objects with names of the form
#'   `{stat}_{window}` (e.g. `sum_12`, `chain_ytd`).
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom RcppRoll roll_sum roll_sd roll_min roll_max roll_prod
#' @importFrom stats is.ts frequency start time ts sd
#'
#' @details
#' `stats = "sum"` and `stats = "chain"` answer the same question for different
#' kinds of series. For a flow measured in levels (units sold, jobs created),
#' the 12-month accumulation is the sum. For a series that is already a rate of
#' change (monthly inflation, monthly returns), summing is only an
#' approximation; the correct accumulation compounds the rates:
#' \deqn{(1 + r_1)(1 + r_2)\cdots(1 + r_k) - 1}
#'
#' Note that a rolling sum is proportional to the simple moving average
#' available through [extract_trends()]: `roll_series(x, "sum", window = k)`
#' equals `k` times `extract_trends(x, "ma", window = k, align = "right")`. The
#' rolling version is the one to reach for when the accumulated quantity is
#' itself the number of interest.
#'
#' @seealso [augment_rolling()] for the data frame interface,
#'   [extract_trends()] for trend estimation.
#'
#' @examples
#' # 12-month rolling sum of vehicle production
#' prod_ts <- df_to_ts(vehicles, value_col = "production", frequency = 12)
#' roll_series(prod_ts, "sum", window = 12)
#'
#' # Accumulated growth over 12 months, from monthly rates in percent
#' ibc_ts <- df_to_ts(ibcbr, value_col = "index", frequency = 12)
#' rates <- 100 * (ibc_ts / stats::lag(ibc_ts, -1) - 1)
#' roll_series(rates, "chain", window = 12, percent = TRUE)
#'
#' # Year-to-date accumulation, resetting each January
#' roll_series(rates, "chain", window = "ytd", percent = TRUE)
#'
#' # Several statistics and windows at once
#' roll_series(prod_ts, stats = c("sum", "sd"), window = c(3, 12))
#'
#' @export
roll_series <- function(
  ts_data,
  stats = "sum",
  window = NULL,
  align = "right",
  percent = FALSE,
  na_rm = FALSE,
  .quiet = FALSE
) {
  # Convert to ts object using tsbox if needed
  if (!stats::is.ts(ts_data)) {
    tryCatch(
      {
        ts_data <- tsbox::ts_ts(ts_data)
      },
      error = function(e) {
        cli::cli_abort(c(
          "Failed to convert input to time series object.",
          "i" = "Input must be convertible to ts via the tsbox package.",
          "x" = "Error: {e$message}"
        ))
      }
    )
  }

  freq <- stats::frequency(ts_data)
  window <- .validate_rolling_params(
    stats = stats,
    window = window,
    align = align,
    percent = percent,
    na_rm = na_rm,
    freq = freq,
    n = length(ts_data)
  )

  .validate_chain_scale(ts_data, stats, percent, .quiet)

  # One result per stat x window combination
  results <- list()
  for (w in window) {
    for (stat in stats) {
      if (!.quiet) {
        .inform_rolling(stat, w, align)
      }
      results[[paste0(stat, "_", w)]] <- .roll_one(
        ts_data = ts_data,
        stat = stat,
        window = w,
        align = align,
        percent = percent,
        na_rm = na_rm
      )
    }
  }

  if (length(results) == 1) {
    return(results[[1]])
  }

  return(results)
}

## Validation ----------------------------------------------------------------

#' Validate rolling arguments and normalise `window`
#'
#' @description Returns the normalised window, either a numeric vector or the
#' single string `"ytd"`. Aborts on invalid input.
#' @noRd
.validate_rolling_params <- function(
  stats,
  window,
  align,
  percent,
  na_rm,
  freq,
  n
) {
  if (!is.character(stats) || length(stats) == 0 || anyNA(stats)) {
    cli::cli_abort("{.arg stats} must be a non-empty character vector")
  }

  valid_stats <- .valid_rolling_stats()
  invalid <- setdiff(stats, valid_stats)
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "Invalid rolling statistic{?s}: {.val {invalid}}",
      "i" = "Available statistics: {.val {valid_stats}}"
    ))
  }

  if (anyDuplicated(stats) > 0) {
    cli::cli_abort("{.arg stats} must not contain duplicates")
  }

  if (!align %in% c("left", "center", "right")) {
    cli::cli_abort(
      "{.arg align} must be one of 'left', 'center', or 'right', got {.val {align}}"
    )
  }

  if (!is.logical(percent) || length(percent) != 1 || is.na(percent)) {
    cli::cli_abort("{.arg percent} must be a single {.code TRUE} or {.code FALSE}")
  }

  if (!is.logical(na_rm) || length(na_rm) != 1 || is.na(na_rm)) {
    cli::cli_abort("{.arg na_rm} must be a single {.code TRUE} or {.code FALSE}")
  }

  # Default window follows the series frequency
  if (is.null(window)) {
    if (freq < 2) {
      cli::cli_abort(c(
        "{.arg window} must be supplied for series with frequency {freq}.",
        "i" = "Frequency-based defaults are only available for seasonal data."
      ))
    }
    return(freq)
  }

  if (is.character(window)) {
    if (length(window) != 1 || !identical(window, "ytd")) {
      cli::cli_abort(c(
        "The only character value accepted by {.arg window} is {.val ytd}.",
        "i" = "Numeric windows and {.val ytd} cannot be combined in one call."
      ))
    }
    return(window)
  }

  if (!is.numeric(window) || length(window) == 0 || anyNA(window)) {
    cli::cli_abort(
      "{.arg window} must be a positive integer, a vector of positive integers, or {.val ytd}"
    )
  }

  if (any(window < 2) || any(window != round(window))) {
    cli::cli_abort(
      "{.arg window} values must be whole numbers of at least 2, got {.val {window}}"
    )
  }

  if (anyDuplicated(window) > 0) {
    cli::cli_abort("{.arg window} must not contain duplicates")
  }

  too_long <- window[window > n]
  if (length(too_long) > 0) {
    cli::cli_abort(c(
      "Rolling window cannot exceed the series length of {n} observations.",
      "i" = "Too large: {.val {too_long}}"
    ))
  }

  return(as.integer(window))
}

#' Largest plausible decimal rate before the series looks like percentages
#' @noRd
.CHAIN_DECIMAL_MAX <- 0.5

#' Largest percentage-point rate before the series looks like decimals
#' @noRd
.CHAIN_PERCENT_MIN <- 0.05

#' Warn when chained rates look mis-scaled
#'
#' @description `chain` is the one statistic where a 100x scaling mistake is
#' silent, so the plausibility of the assumed scale is checked against the
#' largest absolute rate in the series. A decimal rate above 50% per period, or
#' a percentage-point rate that never reaches 0.05, almost always means
#' `percent` was set the wrong way round. Both thresholds are deliberately
#' loose: the check only warns, and never changes the result.
#' @noRd
.validate_chain_scale <- function(ts_data, stats, percent, .quiet) {
  if (.quiet || !"chain" %in% stats) {
    return(invisible(NULL))
  }

  v <- as.numeric(ts_data)
  v <- v[!is.na(v)]
  if (length(v) == 0) {
    return(invisible(NULL))
  }

  peak <- max(abs(v))

  if (!percent && peak > .CHAIN_DECIMAL_MAX) {
    cli::cli_warn(c(
      "Values passed to {.val chain} look like percentages, not decimals.",
      "i" = "With {.code percent = FALSE} the largest rate ({signif(peak, 4)}) is read as {signif(peak * 100, 4)}% per period.",
      "i" = "Set {.code percent = TRUE} if the series is in percentage points."
    ))
  }

  if (percent && peak < .CHAIN_PERCENT_MIN) {
    cli::cli_warn(c(
      "Values passed to {.val chain} look like decimals, not percentages.",
      "i" = "With {.code percent = TRUE} the largest rate ({signif(peak, 4)}) is read as {signif(peak, 4)}% per period.",
      "i" = "Set {.code percent = FALSE} if the series holds decimal rates."
    ))
  }

  return(invisible(NULL))
}

#' Report the rolling computation about to run
#' @noRd
.inform_rolling <- function(stat, window, align) {
  if (identical(window, "ytd")) {
    cli::cli_inform("Computing year-to-date {stat}")
  } else {
    cli::cli_inform(
      "Computing {window}-period rolling {stat} with {align} alignment"
    )
  }

  return(invisible(NULL))
}

## Computation ---------------------------------------------------------------

#' Compute one rolling statistic and return it as a ts
#' @noRd
.roll_one <- function(ts_data, stat, window, align, percent, na_rm) {
  v <- as.numeric(ts_data)

  if (identical(window, "ytd")) {
    result <- .roll_ytd(v, ts_data, stat, percent, na_rm)
  } else {
    result <- .roll_fixed(v, stat, window, align, percent, na_rm)
  }

  out <- stats::ts(
    result,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(out)
}

#' Fixed-width rolling window via RcppRoll
#' @noRd
.roll_fixed <- function(v, stat, window, align, percent, na_rm) {
  if (stat == "chain") {
    rates <- if (percent) v / 100 else v
    prod <- RcppRoll::roll_prod(
      1 + rates,
      n = window,
      align = align,
      fill = NA,
      na.rm = na_rm
    )
    out <- prod - 1
    if (percent) {
      out <- out * 100
    }
    return(out)
  }

  roll_fun <- switch(
    stat,
    "sum" = RcppRoll::roll_sum,
    "mean" = RcppRoll::roll_mean,
    "sd" = RcppRoll::roll_sd,
    "min" = RcppRoll::roll_min,
    "max" = RcppRoll::roll_max
  )

  out <- roll_fun(v, n = window, align = align, fill = NA, na.rm = na_rm)
  return(out)
}

#' Expanding year-to-date accumulation, resetting at each new year
#' @noRd
.roll_ytd <- function(v, ts_data, stat, percent, na_rm) {
  # floor() of the ts time index gives the calendar year of each observation
  year <- as.integer(floor(stats::time(ts_data) + 1e-8))

  out <- rep(NA_real_, length(v))
  for (y in unique(year)) {
    idx <- which(year == y)
    out[idx] <- .expanding_stat(v[idx], stat, percent, na_rm)
  }

  return(out)
}

#' Expanding-window statistic over a single year's observations
#' @noRd
.expanding_stat <- function(v, stat, percent, na_rm) {
  n <- length(v)

  if (stat == "sd") {
    # No closed cumulative form; years hold at most `frequency` observations
    out <- vapply(
      seq_len(n),
      function(i) {
        w <- v[seq_len(i)]
        if (!na_rm && anyNA(w)) {
          return(NA_real_)
        }
        w <- w[!is.na(w)]
        if (length(w) < 2) {
          return(NA_real_)
        }
        return(stats::sd(w))
      },
      numeric(1)
    )
    return(out)
  }

  if (stat == "chain") {
    rates <- if (percent) v / 100 else v
    x <- 1 + rates
    if (na_rm) {
      x[is.na(x)] <- 1
    }
    out <- cumprod(x) - 1
    if (percent) {
      out <- out * 100
    }
    return(out)
  }

  if (stat == "sum") {
    x <- v
    if (na_rm) {
      x[is.na(x)] <- 0
    }
    return(cumsum(x))
  }

  if (stat == "mean") {
    x <- v
    if (na_rm) {
      observed <- !is.na(x)
      x[!observed] <- 0
      out <- cumsum(x) / cumsum(observed)
      out[is.nan(out)] <- NA_real_
      return(out)
    }
    return(cumsum(x) / seq_len(n))
  }

  # min and max
  if (!na_rm) {
    return(if (stat == "min") cummin(v) else cummax(v))
  }

  compare <- if (stat == "min") min else max
  out <- Reduce(
    function(acc, new) {
      if (is.na(new)) {
        return(acc)
      }
      if (is.na(acc)) {
        return(new)
      }
      return(compare(acc, new))
    },
    v,
    accumulate = TRUE
  )
  return(as.numeric(unlist(out)))
}
