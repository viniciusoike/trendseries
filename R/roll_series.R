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
#'   economic indicators. Ignored when `window = "ytd"`. An even window has no
#'   exact centre; see Details for how each statistic handles that.
#' @param percent Only used by `stats = "chain"`. If `FALSE` (default), rates
#'   are assumed to be decimals (0.005 for 0.5%). If `TRUE`, rates are assumed
#'   to be percentages (0.5 for 0.5%) and the result is returned in percent.
#' @param na_rm If `TRUE`, missing values are ignored within each window. The
#'   default `FALSE` propagates `NA`, so an incomplete window yields `NA`. A
#'   window holding no observed values yields `NA` either way, as does a
#'   window holding one value for `"sd"`.
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
#' itself the number of interest. The two part company for an even `window`
#' under `align = "center"`, where the moving average is weighted and the sum
#' is not.
#'
#' An even window centred on an observation has one more period on one side
#' than the other. `"mean"` resolves this the way the `ma` trend method does,
#' with the 2xN filter that puts half weight on the two endpoints, so
#' `roll_series(x, "mean", window = k, align = "center")` matches
#' `extract_trends(x, "ma", window = k, align = "center")`. The other
#' statistics have no such correction and use a window with one extra period
#' after the anchor.
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
  results <- .roll_series_list(
    ts_data = ts_data,
    stats = stats,
    window = window,
    align = align,
    percent = percent,
    na_rm = na_rm,
    .quiet = .quiet
  )

  if (length(results) == 1) {
    return(results[[1]])
  }

  return(results)
}

#' Rolling aggregations, always as a named list
#'
#' @description The list is the real return value; `roll_series()` only unwraps
#' a single result for convenience. `augment_rolling()` calls this instead, so
#' the `{stat}_{window}` names that become column names are built in one place.
#'
#' `.check_inputs` exists for the grouped path, which runs the scale and
#' calendar checks once on the whole input rather than once per group.
#' @noRd
.roll_series_list <- function(
  ts_data,
  stats = "sum",
  window = NULL,
  align = "right",
  percent = FALSE,
  na_rm = FALSE,
  .quiet = FALSE,
  .check_inputs = TRUE
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

  if (.check_inputs) {
    .warn_ignored_args(stats, window, align, percent)
    .validate_chain_scale(as.numeric(ts_data), stats, percent)
    if (identical(window, "ytd")) {
      .warn_ytd_partial_start(.ts_start_period(ts_data), freq)
    }
  }

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

  # Both the frequency default and "ytd" need a calendar to divide the year
  if (is.null(window) || identical(window, "ytd")) {
    if (freq < 2) {
      what <- if (is.null(window)) "The default {.arg window}" else "{.val ytd}"
      cli::cli_abort(c(
        paste(what, "is not available for series with frequency {freq}."),
        "i" = "A seasonal frequency is needed to divide the year into periods.",
        "i" = "Supply a numeric {.arg window} instead."
      ))
    }
  }

  if (is.null(window)) {
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

#' Warn about arguments the requested combination ignores
#'
#' @description `align` has no meaning for an expanding year-to-date window,
#' and `percent` is read only by `chain`. Both were dropped silently before.
#' @noRd
.warn_ignored_args <- function(stats, window, align, percent) {
  if (identical(window, "ytd") && !identical(align, "right")) {
    cli::cli_warn(c(
      "{.arg align} is ignored when {.arg window} is {.val ytd}.",
      "i" = "A year-to-date window expands from the start of the year."
    ))
  }

  if (isTRUE(percent) && !"chain" %in% stats) {
    cli::cli_warn(c(
      "{.arg percent} is ignored by {.val {stats}}.",
      "i" = "Only {.val chain} reads it, to decide whether rates are decimals or percentage points."
    ))
  }

  return(invisible(NULL))
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
#'
#' Not gated on `.quiet`, which suppresses narration rather than correctness
#' signals. The grouped path runs this once on the whole input.
#' @noRd
.validate_chain_scale <- function(values, stats, percent) {
  if (!"chain" %in% stats) {
    return(invisible(NULL))
  }

  v <- as.numeric(values)
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

#' Period of the year the series starts in, counting from 1
#' @noRd
.ts_start_period <- function(ts_data) {
  return(as.integer(stats::start(ts_data)[2]))
}

#' Warn when a year-to-date accumulation starts mid-year
#'
#' @description The first year of a series beginning in, say, March accumulates
#' from March, not from January, so it is not comparable with the years that
#' follow. The values are left alone; the reader is the one who has to know.
#' @noRd
.warn_ytd_partial_start <- function(start_period, freq) {
  if (is.na(start_period) || start_period == 1) {
    return(invisible(NULL))
  }

  unit <- if (freq == 12) "month" else if (freq == 4) "quarter" else "period"

  cli::cli_warn(c(
    "Series starts at {unit} {start_period}, so the first year is incomplete.",
    "i" = "Its year-to-date values accumulate from {unit} {start_period} onwards, not from the start of the year.",
    "i" = "They are not comparable with later years."
  ))

  return(invisible(NULL))
}

#' Report the rolling computation about to run
#' @noRd
.inform_rolling <- function(stat, window, align) {
  if (identical(window, "ytd")) {
    cli::cli_inform("Computing year-to-date {stat}")
  } else if (stat == "mean" && .use_2xn(window, align)) {
    cli::cli_inform(
      "Computing 2x{window}-period rolling mean (auto-adjusted for even-window centering)"
    )
  } else {
    cli::cli_inform(
      "Computing {window}-period rolling {stat} with {align} alignment"
    )
  }

  return(invisible(NULL))
}

## Computation ---------------------------------------------------------------

#' Observations a statistic needs before it has an answer
#' @noRd
.min_obs <- function(stat) {
  return(if (stat == "sd") 2L else 1L)
}

#' Blank windows that hold too few observations
#'
#' @description With `na_rm = TRUE` the backends return their identity element
#' for a window with nothing in it: `0` for a sum, `1` for a product, `Inf` and
#' `-Inf` for a minimum and a maximum, `NaN` for a mean. None of those are
#' answers, so every statistic is blanked by the same rule. With
#' `na_rm = FALSE` the backends already propagate `NA` and this is a no-op,
#' which is why it runs unconditionally rather than on a branch.
#' @noRd
.blank_short_windows <- function(out, counts, stat) {
  short <- is.na(counts) | counts < .min_obs(stat)
  out[short] <- NA_real_

  return(out)
}

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
  counts <- RcppRoll::roll_sum(
    as.numeric(!is.na(v)),
    n = window,
    align = align,
    fill = NA
  )

  out <- .roll_fixed_raw(v, stat, window, align, percent, na_rm)

  return(.blank_short_windows(out, counts, stat))
}

#' Dispatch a fixed-width window to its backend, before the window count check
#' @noRd
.roll_fixed_raw <- function(v, stat, window, align, percent, na_rm) {
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

  # An even centered mean is the 2xN filter, matching the `ma` trend method
  if (stat == "mean" && .use_2xn(window, align)) {
    return(.ma_2xn(v, window))
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

#' Does this window and alignment call for the 2xN correction?
#' @noRd
.use_2xn <- function(window, align) {
  if (identical(window, "ytd")) {
    return(FALSE)
  }

  return(window %% 2 == 0 && align == "center")
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
  out <- .expanding_stat_raw(v, stat, percent, na_rm)

  return(.blank_short_windows(out, cumsum(!is.na(v)), stat))
}

#' Expanding-window statistic before the window count check
#' @noRd
.expanding_stat_raw <- function(v, stat, percent, na_rm) {
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
      return(cumsum(x) / cumsum(observed))
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
