## Helpers -------------------------------------------------------------------

prod_ts <- function() {
  df_to_ts(vehicles, value_col = "production", frequency = 12)
}

# Twelve months of a constant 1% rate, in percentage points
rate_ts <- function(rate = 1, n = 24) {
  stats::ts(rep(rate, n), start = c(2020, 1), frequency = 12)
}

## Basic behaviour -----------------------------------------------------------

test_that("roll_series() returns a ts of the same length", {
  x <- prod_ts()
  result <- roll_series(x, "sum", window = 12, .quiet = TRUE)

  expect_s3_class(result, "ts")
  expect_equal(length(result), length(x))
  expect_equal(stats::start(result), stats::start(x))
  expect_equal(stats::frequency(result), stats::frequency(x))
})

test_that("a single stat and window returns a bare ts, more returns a list", {
  x <- prod_ts()

  expect_s3_class(roll_series(x, "sum", window = 12, .quiet = TRUE), "ts")

  two_stats <- roll_series(x, c("sum", "mean"), window = 12, .quiet = TRUE)
  expect_type(two_stats, "list")
  expect_named(two_stats, c("sum_12", "mean_12"))

  two_windows <- roll_series(x, "sum", window = c(3, 12), .quiet = TRUE)
  expect_type(two_windows, "list")
  expect_named(two_windows, c("sum_3", "sum_12"))

  grid <- roll_series(x, c("sum", "sd"), window = c(3, 12), .quiet = TRUE)
  expect_named(grid, c("sum_3", "sd_3", "sum_12", "sd_12"))
  expect_true(all(vapply(grid, stats::is.ts, logical(1))))
})

test_that("window defaults to the series frequency", {
  monthly <- roll_series(prod_ts(), "sum", .quiet = TRUE)
  explicit_monthly <- roll_series(prod_ts(), "sum", window = 12, .quiet = TRUE)
  expect_equal(as.numeric(monthly), as.numeric(explicit_monthly))

  gdp <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)
  quarterly <- roll_series(gdp, "sum", .quiet = TRUE)
  explicit_quarterly <- roll_series(gdp, "sum", window = 4, .quiet = TRUE)
  expect_equal(as.numeric(quarterly), as.numeric(explicit_quarterly))
})

## Correctness of each statistic ---------------------------------------------

test_that("rolling sum equals k times the right-aligned moving average", {
  x <- prod_ts()
  rolled <- roll_series(x, "sum", window = 12, .quiet = TRUE)
  ma <- extract_trends(
    x,
    methods = "ma",
    window = 12,
    align = "right",
    .quiet = TRUE
  )

  expect_equal(as.numeric(rolled), 12 * as.numeric(ma))
})

test_that("right-aligned windows leave n - 1 leading NAs", {
  x <- prod_ts()

  expect_equal(
    sum(is.na(roll_series(x, "sum", window = 12, .quiet = TRUE))),
    11
  )
  expect_equal(sum(is.na(roll_series(x, "sum", window = 3, .quiet = TRUE))), 2)

  # The first complete window is the sum of the first 12 observations
  result <- roll_series(x, "sum", window = 12, .quiet = TRUE)
  expect_equal(as.numeric(result)[12], sum(as.numeric(x)[1:12]))
})

test_that("mean, sd, min and max match their base R equivalents", {
  x <- prod_ts()
  v <- as.numeric(x)
  idx <- 40:45

  rolled <- roll_series(
    x,
    c("mean", "sd", "min", "max"),
    window = 6,
    .quiet = TRUE
  )

  for (i in idx) {
    w <- v[(i - 5):i]
    expect_equal(as.numeric(rolled$mean_6)[i], mean(w))
    expect_equal(as.numeric(rolled$sd_6)[i], stats::sd(w))
    expect_equal(as.numeric(rolled$min_6)[i], min(w))
    expect_equal(as.numeric(rolled$max_6)[i], max(w))
  }
})

test_that("chain compounds rates rather than summing them", {
  x <- rate_ts(rate = 1)
  chained <- roll_series(x, "chain", window = 12, percent = TRUE, .quiet = TRUE)

  expected <- (1.01^12 - 1) * 100
  expect_equal(as.numeric(chained)[12], expected)

  # Compounding exceeds the naive sum of the rates
  summed <- roll_series(x, "sum", window = 12, .quiet = TRUE)
  expect_gt(as.numeric(chained)[12], as.numeric(summed)[12])
  expect_equal(as.numeric(summed)[12], 12)
})

test_that("chain honours the percent argument", {
  as_percent <- roll_series(
    rate_ts(rate = 1),
    "chain",
    window = 12,
    percent = TRUE,
    .quiet = TRUE
  )
  as_decimal <- roll_series(
    rate_ts(rate = 0.01),
    "chain",
    window = 12,
    percent = FALSE,
    .quiet = TRUE
  )

  expect_equal(as.numeric(as_percent)[12], as.numeric(as_decimal)[12] * 100)
})

test_that("chain over one period reproduces the input rate", {
  x <- stats::ts(c(1, 2, -1, 0.5, 3, 1), start = c(2020, 1), frequency = 12)
  chained <- roll_series(x, "chain", window = 2, percent = TRUE, .quiet = TRUE)

  # Two-period compounding of 1% then 2%
  expect_equal(as.numeric(chained)[2], (1.01 * 1.02 - 1) * 100)
  # Negative rates compound correctly
  expect_equal(as.numeric(chained)[3], (1.02 * 0.99 - 1) * 100)
})

## Alignment ------------------------------------------------------------------

test_that("align shifts where the NAs fall", {
  x <- prod_ts()
  n <- length(x)

  right <- roll_series(x, "sum", window = 5, align = "right", .quiet = TRUE)
  left <- roll_series(x, "sum", window = 5, align = "left", .quiet = TRUE)
  center <- roll_series(x, "sum", window = 5, align = "center", .quiet = TRUE)

  expect_true(all(is.na(as.numeric(right)[1:4])))
  expect_false(is.na(as.numeric(right)[5]))

  expect_true(all(is.na(as.numeric(left)[(n - 3):n])))
  expect_false(is.na(as.numeric(left)[1]))

  expect_true(all(is.na(as.numeric(center)[c(1, 2, n - 1, n)])))

  # Same underlying windows, just repositioned
  expect_equal(as.numeric(right)[5], as.numeric(left)[1])
  expect_equal(as.numeric(right)[5], as.numeric(center)[3])
})

## Year-to-date ---------------------------------------------------------------

test_that("ytd sum accumulates within a year and resets at the boundary", {
  # Starts in November so the first year holds only two observations
  x <- stats::ts(1:15, start = c(2020, 11), frequency = 12)
  result <- suppressWarnings(
    roll_series(x, "sum", window = "ytd", .quiet = TRUE)
  )

  expect_equal(as.numeric(result)[1:2], c(1, 3))
  expect_equal(as.numeric(result)[3], 3)
  expect_equal(as.numeric(result)[14], sum(3:14))
  expect_equal(as.numeric(result)[15], 15)
})

test_that("ytd chain compounds within the year", {
  x <- rate_ts(rate = 1, n = 14)
  result <- roll_series(
    x,
    "chain",
    window = "ytd",
    percent = TRUE,
    .quiet = TRUE
  )

  expect_equal(as.numeric(result)[1], 1)
  expect_equal(as.numeric(result)[12], (1.01^12 - 1) * 100)
  # January of the next year restarts the accumulation
  expect_equal(as.numeric(result)[13], 1)
  expect_equal(as.numeric(result)[14], (1.01^2 - 1) * 100)
})

test_that("ytd works with quarterly data", {
  x <- stats::ts(rep(1, 10), start = c(2020, 1), frequency = 4)
  result <- roll_series(x, "sum", window = "ytd", .quiet = TRUE)

  expect_equal(as.numeric(result), c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2))
})

test_that("ytd supports the other statistics", {
  x <- stats::ts(c(4, 2, 6, 8), start = c(2020, 1), frequency = 4)

  expect_equal(
    as.numeric(roll_series(x, "mean", window = "ytd", .quiet = TRUE)),
    c(4, 3, 4, 5)
  )
  expect_equal(
    as.numeric(roll_series(x, "min", window = "ytd", .quiet = TRUE)),
    c(4, 2, 2, 2)
  )
  expect_equal(
    as.numeric(roll_series(x, "max", window = "ytd", .quiet = TRUE)),
    c(4, 4, 6, 8)
  )

  sd_result <- as.numeric(roll_series(x, "sd", window = "ytd", .quiet = TRUE))
  expect_true(is.na(sd_result[1]))
  expect_equal(sd_result[2], stats::sd(c(4, 2)))
  expect_equal(sd_result[4], stats::sd(c(4, 2, 6, 8)))
})

test_that("ytd results are named with the ytd suffix", {
  x <- rate_ts()
  result <- roll_series(x, c("sum", "mean"), window = "ytd", .quiet = TRUE)

  expect_named(result, c("sum_ytd", "mean_ytd"))
})

## Missing values -------------------------------------------------------------

test_that("na_rm = FALSE propagates NA through the window", {
  x <- stats::ts(c(1, 2, NA, 4, 5, 6), start = c(2020, 1), frequency = 12)
  result <- as.numeric(roll_series(x, "sum", window = 3, .quiet = TRUE))

  expect_true(all(is.na(result[1:5])))
  expect_equal(result[6], 15)
})

test_that("na_rm = TRUE aggregates over the observed values", {
  x <- stats::ts(c(1, 2, NA, 4, 5, 6), start = c(2020, 1), frequency = 12)
  result <- as.numeric(
    roll_series(x, "sum", window = 3, na_rm = TRUE, .quiet = TRUE)
  )

  expect_equal(result[3], 3)
  expect_equal(result[4], 6)
  expect_equal(result[6], 15)
})

test_that("na_rm is respected by the ytd path", {
  x <- stats::ts(c(1, NA, 3, 4), start = c(2020, 1), frequency = 4)

  propagated <- as.numeric(roll_series(x, "sum", window = "ytd", .quiet = TRUE))
  expect_equal(propagated[1], 1)
  expect_true(all(is.na(propagated[2:4])))

  ignored <- as.numeric(
    roll_series(x, "sum", window = "ytd", na_rm = TRUE, .quiet = TRUE)
  )
  expect_equal(ignored, c(1, 1, 4, 8))

  running_max <- as.numeric(
    roll_series(x, "max", window = "ytd", na_rm = TRUE, .quiet = TRUE)
  )
  expect_equal(running_max, c(1, 1, 3, 4))

  running_mean <- as.numeric(
    roll_series(x, "mean", window = "ytd", na_rm = TRUE, .quiet = TRUE)
  )
  expect_equal(running_mean, c(1, 1, 2, 8 / 3))
})

test_that("a leading NA does not poison the ytd mean when na_rm = TRUE", {
  x <- stats::ts(c(NA, 2, 4), start = c(2020, 1), frequency = 4)
  result <- as.numeric(
    roll_series(x, "mean", window = "ytd", na_rm = TRUE, .quiet = TRUE)
  )

  expect_true(is.na(result[1]))
  expect_equal(result[2:3], c(2, 3))
})

## Input handling -------------------------------------------------------------

test_that("non-ts input is converted via tsbox", {
  df <- ts_to_df(prod_ts())
  result <- roll_series(df, "sum", window = 12, .quiet = TRUE)

  expect_s3_class(result, "ts")
  expect_equal(
    as.numeric(result),
    as.numeric(roll_series(prod_ts(), "sum", window = 12, .quiet = TRUE))
  )
})

## Validation -----------------------------------------------------------------

test_that("invalid statistics are rejected with the available options", {
  x <- prod_ts()

  expect_error(
    roll_series(x, "bogus", .quiet = TRUE),
    "Invalid rolling statistic"
  )
  expect_error(roll_series(x, "bogus", .quiet = TRUE), "sum")
  expect_error(roll_series(x, character(0), .quiet = TRUE), "non-empty")
  expect_error(roll_series(x, c("sum", "sum"), .quiet = TRUE), "duplicates")
})

test_that("invalid windows are rejected", {
  x <- prod_ts()

  expect_error(roll_series(x, "sum", window = 1, .quiet = TRUE), "at least 2")
  expect_error(
    roll_series(x, "sum", window = 4.5, .quiet = TRUE),
    "whole numbers"
  )
  expect_error(roll_series(x, "sum", window = -3, .quiet = TRUE), "at least 2")
  expect_error(
    roll_series(x, "sum", window = c(3, 3), .quiet = TRUE),
    "duplicates"
  )
  expect_error(
    roll_series(x, "sum", window = length(x) + 1, .quiet = TRUE),
    "cannot exceed the series length"
  )
  expect_error(
    roll_series(x, "sum", window = c(3, length(x) + 1), .quiet = TRUE),
    "cannot exceed the series length"
  )
})

test_that("only 'ytd' is accepted as a character window", {
  x <- prod_ts()

  expect_error(roll_series(x, "sum", window = "yoy", .quiet = TRUE), "ytd")
  expect_error(
    roll_series(x, "sum", window = c("ytd", "ytd"), .quiet = TRUE),
    "ytd"
  )
})

test_that("other arguments are validated", {
  x <- prod_ts()

  expect_error(roll_series(x, "sum", align = "middle", .quiet = TRUE), "align")
  expect_error(roll_series(x, "sum", percent = NA, .quiet = TRUE), "percent")
  expect_error(roll_series(x, "sum", na_rm = "yes", .quiet = TRUE), "na_rm")
})

test_that("annual series require an explicit numeric window", {
  x <- stats::ts(1:10, start = 2000, frequency = 1)

  expect_error(roll_series(x, "sum", .quiet = TRUE), "default `window`")
  expect_error(
    roll_series(x, "sum", window = "ytd", .quiet = TRUE),
    "not available for series with frequency 1"
  )
  expect_s3_class(roll_series(x, "sum", window = 3, .quiet = TRUE), "ts")
})

## Scale guard ----------------------------------------------------------------

test_that("chain warns when percent = FALSE but rates look like percentages", {
  expect_warning(
    roll_series(rate_ts(rate = 1), "chain", window = 12, percent = FALSE),
    "percent = TRUE"
  )
  expect_warning(
    roll_series(prod_ts(), "chain", window = 12, percent = FALSE),
    "look like percentages"
  )
})

test_that("chain warns when percent = TRUE but rates look like decimals", {
  expect_warning(
    roll_series(rate_ts(rate = 0.01), "chain", window = 12, percent = TRUE),
    "percent = FALSE"
  )
})

test_that("chain stays silent when the rate scale is plausible", {
  # 1% per month, correctly declared as percentage points
  expect_no_warning(
    roll_series(rate_ts(rate = 1), "chain", window = 12, percent = TRUE)
  )
  # The same rate as a decimal, correctly declared
  expect_no_warning(
    roll_series(rate_ts(rate = 0.01), "chain", window = 12, percent = FALSE)
  )
  # Low but realistic percentage-point inflation stays under the threshold
  expect_no_warning(
    roll_series(rate_ts(rate = 0.2), "chain", window = 12, percent = TRUE)
  )
})

test_that("the scale guard only applies to chain", {
  expect_no_warning(roll_series(prod_ts(), "sum", window = 12))
})

test_that(".quiet suppresses progress messages but not the scale warning", {
  expect_message(
    roll_series(prod_ts(), "sum", window = 12),
    "12-period rolling sum"
  )
  expect_silent(roll_series(prod_ts(), "sum", window = 12, .quiet = TRUE))

  # .quiet means "do not narrate", not "do not report a problem"
  expect_warning(
    roll_series(rate_ts(rate = 1), "chain", window = 12, .quiet = TRUE),
    "look like percentages"
  )
})

test_that("year-to-date narration is suppressed by .quiet", {
  x <- stats::ts(1:24, start = c(2020, 1), frequency = 12)

  expect_message(roll_series(x, "sum", window = "ytd"), "year-to-date")
  expect_silent(roll_series(x, "sum", window = "ytd", .quiet = TRUE))
})

## Windows with too few observations ------------------------------------------

test_that("a window holding no observations yields NA, not an identity value", {
  # Four leading gaps, so the right-aligned 3-period windows at positions 3
  # and 4 see nothing at all
  x <- stats::ts(c(NA, NA, NA, NA, 1:8), start = c(2020, 1), frequency = 12)

  for (stat in c("sum", "mean", "sd", "min", "max", "chain")) {
    result <- suppressWarnings(
      roll_series(x, stat, window = 3, na_rm = TRUE, .quiet = TRUE)
    )
    expect_equal(
      as.numeric(result)[3:4],
      c(NA_real_, NA_real_),
      info = stat
    )
    expect_false(any(is.infinite(as.numeric(result))), info = stat)
    expect_false(any(is.nan(as.numeric(result))), info = stat)
  }
})

test_that("an empty expanding window yields NA for every statistic", {
  # January is missing, so the year-to-date value there rests on no observation
  x <- stats::ts(c(NA, 2:12), start = c(2020, 1), frequency = 12)

  for (stat in c("sum", "mean", "sd", "min", "max", "chain")) {
    result <- suppressWarnings(
      roll_series(x, stat, window = "ytd", na_rm = TRUE, .quiet = TRUE)
    )
    expect_true(is.na(as.numeric(result)[1]), info = stat)
    expect_false(any(is.infinite(as.numeric(result))), info = stat)
    expect_false(any(is.nan(as.numeric(result))), info = stat)
  }
})

test_that("sd needs two observations before it reports a value", {
  x <- stats::ts(c(NA, NA, 3, 4, 5, 6), start = c(2020, 1), frequency = 12)

  fixed <- roll_series(x, "sd", window = 3, na_rm = TRUE, .quiet = TRUE)
  # Position 3 holds one observation, position 4 holds two
  expect_true(is.na(as.numeric(fixed)[3]))
  expect_equal(as.numeric(fixed)[4], stats::sd(c(3, 4)))

  expanding <- roll_series(x, "sd", window = "ytd", na_rm = TRUE, .quiet = TRUE)
  expect_true(is.na(as.numeric(expanding)[3]))
  expect_equal(as.numeric(expanding)[4], stats::sd(c(3, 4)))
})

test_that("na_rm = FALSE still propagates NA through the window", {
  x <- stats::ts(c(1, 2, NA, 4, 5, 6), start = c(2020, 1), frequency = 12)
  result <- roll_series(x, "sum", window = 3, .quiet = TRUE)

  expect_equal(as.numeric(result), c(NA, NA, NA, NA, NA, 15))
})

## Even centred windows -------------------------------------------------------

test_that("an even centred mean matches the ma trend method", {
  x <- prod_ts()

  for (k in c(4, 12)) {
    expect_equal(
      as.numeric(roll_series(
        x,
        "mean",
        window = k,
        align = "center",
        .quiet = TRUE
      )),
      as.numeric(extract_trends(
        x,
        "ma",
        window = k,
        align = "center",
        .quiet = TRUE
      ))
    )
  }
})

test_that("the 2xN correction applies only to a centred even mean", {
  x <- prod_ts()
  v <- as.numeric(x)

  # Odd windows and non-centred windows stay with the plain rolling mean
  expect_equal(
    as.numeric(roll_series(
      x,
      "mean",
      window = 13,
      align = "center",
      .quiet = TRUE
    )),
    RcppRoll::roll_mean(v, n = 13, align = "center", fill = NA)
  )
  expect_equal(
    as.numeric(roll_series(
      x,
      "mean",
      window = 12,
      align = "right",
      .quiet = TRUE
    )),
    RcppRoll::roll_mean(v, n = 12, align = "right", fill = NA)
  )
  # Other statistics have no such correction
  expect_equal(
    as.numeric(roll_series(
      x,
      "sum",
      window = 12,
      align = "center",
      .quiet = TRUE
    )),
    RcppRoll::roll_sum(v, n = 12, align = "center", fill = NA)
  )
})

test_that("the 2xN mean is announced", {
  expect_message(
    roll_series(prod_ts(), "mean", window = 12, align = "center"),
    "2x12"
  )
})

## Year-to-date calendar ------------------------------------------------------

test_that("a year-to-date series starting mid-year warns", {
  x <- stats::ts(1:18, start = c(2020, 7), frequency = 12)

  expect_warning(
    roll_series(x, "sum", window = "ytd", .quiet = TRUE),
    "first year is incomplete"
  )
  expect_no_warning(
    roll_series(
      stats::ts(1:18, start = c(2020, 1), frequency = 12),
      "sum",
      window = "ytd",
      .quiet = TRUE
    )
  )
})

test_that("the partial-year warning names the quarter for quarterly data", {
  x <- stats::ts(1:12, start = c(2020, 3), frequency = 4)

  expect_warning(
    roll_series(x, "sum", window = "ytd", .quiet = TRUE),
    "quarter 3"
  )
})

## Arguments the combination ignores ------------------------------------------

test_that("align warns when the window is year-to-date", {
  x <- stats::ts(1:24, start = c(2020, 1), frequency = 12)

  expect_warning(
    roll_series(x, "sum", window = "ytd", align = "center", .quiet = TRUE),
    "`align` is ignored"
  )
  expect_no_warning(
    roll_series(x, "sum", window = "ytd", .quiet = TRUE)
  )
})

test_that("percent warns when no statistic reads it", {
  x <- stats::ts(1:24, start = c(2020, 1), frequency = 12)

  expect_warning(
    roll_series(x, "sum", window = 3, percent = TRUE, .quiet = TRUE),
    "`percent` is ignored"
  )
  expect_no_warning(
    roll_series(x, c("sum", "chain"), window = 3, percent = TRUE, .quiet = TRUE)
  )
})
