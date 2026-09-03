test_that("frequency detection works correctly", {
  # Test quarterly data
  quarterly_dates <- seq(
    as.Date("2000-01-01"),
    as.Date("2010-01-01"),
    by = "quarter"
  )
  freq_q <- .detect_frequency(quarterly_dates, .quiet = TRUE)
  expect_equal(freq_q, 4)

  # Test monthly data
  monthly_dates <- seq(
    as.Date("2000-01-01"),
    as.Date("2005-01-01"),
    by = "month"
  )
  freq_m <- .detect_frequency(monthly_dates, .quiet = TRUE)
  expect_equal(freq_m, 12)
})

test_that("frequency detection handles irregular dates", {
  # Test with some missing dates (quarterly pattern)
  dates <- seq(as.Date("2000-01-01"), as.Date("2010-01-01"), by = "quarter")
  irregular_dates <- dates[-c(5, 10, 15)] # Remove some dates

  freq <- .detect_frequency(irregular_dates, .quiet = TRUE)
  expect_equal(freq, 4)
})

test_that("frequency detection fails appropriately", {
  # Too few dates
  expect_error(
    .detect_frequency(as.Date("2000-01-01"), .quiet = TRUE),
    "at least 2"
  )

  # Completely irregular dates (now warns but still estimates)
  irregular_dates <- as.Date(c(
    "2000-01-01",
    "2000-02-15",
    "2000-05-20",
    "2000-12-31"
  ))
  expect_warning(
    .detect_frequency(irregular_dates, .quiet = FALSE),
    "Irregular time series detected|Non-standard frequency"
  )

  # Should still return a frequency estimate
  freq <- suppressWarnings(.detect_frequency(irregular_dates, .quiet = TRUE))
  expect_true(is.numeric(freq) && freq > 0)
})

test_that("df_to_ts_internal works correctly", {
  # Test with quarterly data
  result <- .df_to_ts_internal(gdp_construction, "date", "index", 4)
  expect_s3_class(result, "ts")
  expect_equal(frequency(result), 4)

  # Test with monthly data
  result_monthly <- .df_to_ts_internal(ibcbr, "date", "index", 12)
  expect_s3_class(result_monthly, "ts")
  expect_equal(frequency(result_monthly), 12)
})

test_that("df_to_ts_internal drops leading and trailing missing values", {
  # Dropping these does not open an interior gap: `start` is taken from the
  # first retained row, so later observations keep their period positions
  test_data <- gdp_construction
  n <- nrow(test_data)
  test_data$index[1:5] <- NA
  test_data$index[(n - 2):n] <- NA

  result <- .df_to_ts_internal(test_data, "date", "index", 4)
  expect_s3_class(result, "ts")
  expect_equal(length(result), n - 8)
  expect_false(anyNA(result))

  # The retained series starts at the first observed quarter
  expect_equal(
    as.numeric(result)[1],
    gdp_construction$index[6]
  )
})

test_that("df_to_ts_internal rejects interior gaps rather than shifting", {
  # An interior NA would be dropped, shifting every later observation one
  # period earlier and silently misdating the merged results
  na_gap <- gdp_construction
  na_gap$index[20] <- NA
  expect_error(
    .df_to_ts_internal(na_gap, "date", "index", 4),
    "missing period"
  )
  expect_error(
    .df_to_ts_internal(na_gap, "date", "index", 4),
    "had a missing value"
  )

  # A period absent from the data entirely fails the same way
  absent <- gdp_construction[-20, ]
  expect_error(
    .df_to_ts_internal(absent, "date", "index", 4),
    "absent from the data"
  )
})

test_that("df_to_ts_internal rejects duplicated periods", {
  duped <- rbind(gdp_construction, gdp_construction[5, ])

  expect_error(
    .df_to_ts_internal(duped, "date", "index", 4),
    "duplicated period"
  )
})

test_that("the grid check tolerates end-of-month date conventions", {
  # floor_date() normalisation means a month is a month regardless of the
  # day-of-month convention used by the source
  # Built from period starts so February lands on the 29th rather than
  # overflowing into March, which seq.Date() would do from a 31st anchor
  month_starts <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  eom_dates <- lubridate::ceiling_date(month_starts, "month") - 1

  expect_no_error(.check_regular_grid(eom_dates, 12))
  expect_error(.check_regular_grid(eom_dates[-10], 12), "missing period")
})

test_that("the grid check is skipped for frequencies without exact periods", {
  # Weekly and daily series have no exact calendar period, so no claim is made
  weekly <- seq(as.Date("2020-01-01"), by = "week", length.out = 40)

  expect_null(.frequency_unit(52))
  expect_no_error(.check_regular_grid(weekly, 52))
  expect_no_error(.check_regular_grid(weekly[-10], 52))
})

test_that("the grid check reports several gaps compactly", {
  gappy <- gdp_construction[-c(10, 11, 20, 30, 40, 50, 60), ]

  expect_error(
    .df_to_ts_internal(gappy, "date", "index", 4),
    "7 missing periods"
  )
  # Only the first five are listed, with a count for the rest
  expect_error(
    .df_to_ts_internal(gappy, "date", "index", 4),
    "\\(\\+2 more\\)"
  )
})

test_that("trends_to_df works with single trend", {
  # Create a simple ts object
  test_ts <- ts(rnorm(20), frequency = 4, start = c(2000, 1))

  result <- .trends_to_df(test_ts, "date", NULL)
  expect_s3_class(result, "tbl_df")
  expect_true("trend_trend" %in% names(result))
  expect_true("date" %in% names(result))
  expect_equal(nrow(result), 20)
})

test_that("trends_to_df works with multiple trends", {
  # Create multiple ts objects
  test_ts1 <- ts(rnorm(20), frequency = 4, start = c(2000, 1))
  test_ts2 <- ts(rnorm(20), frequency = 4, start = c(2000, 1))

  trends_list <- list(hp = test_ts1, ma = test_ts2)

  result <- .trends_to_df(trends_list, "date", NULL)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("trend_hp", "trend_ma", "date") %in% names(result)))
})

test_that("trends_to_df handles suffix", {
  test_ts <- ts(rnorm(20), frequency = 4, start = c(2000, 1))
  trends_list <- list(hp = test_ts)

  result <- .trends_to_df(trends_list, "date", "test")
  expect_true("trend_hp_test" %in% names(result))
})

test_that("safe_merge works without conflicts", {
  data1 <- tibble::tibble(date = as.Date("2000-01-01") + 0:9, value = rnorm(10))
  data2 <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:9,
    trend_hp = rnorm(10)
  )

  result <- .safe_merge(data1, data2, "date")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("value", "trend_hp") %in% names(result)))
  expect_equal(nrow(result), 10)
})

test_that("safe_merge handles naming conflicts", {
  data1 <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:9,
    value = rnorm(10),
    trend_hp = rnorm(10) # This will conflict
  )
  data2 <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:9,
    trend_hp = rnorm(10)
  )

  expect_warning(
    result <- .safe_merge(data1, data2, "date"),
    "already exists"
  )

  expect_s3_class(result, "tbl_df")
  # Should have original trend_hp and new renamed column
  trend_cols <- grep("trend_hp", names(result), value = TRUE)
  expect_true(length(trend_cols) >= 2)
})

test_that("safe_merge handles NULL trends", {
  data1 <- tibble::tibble(date = as.Date("2000-01-01") + 0:9, value = rnorm(10))

  result <- .safe_merge(data1, NULL, "date")
  expect_identical(result, data1)
})

test_that("validate_economic_frequency works", {
  expect_true(.validate_economic_frequency(4))
  expect_true(.validate_economic_frequency(12))

  expect_error(.validate_economic_frequency(1), "Only monthly.*quarterly")
  expect_error(.validate_economic_frequency(52), "Only monthly.*quarterly")
  expect_error(.validate_economic_frequency(365), "Only monthly.*quarterly")
})
