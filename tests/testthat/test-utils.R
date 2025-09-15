test_that("frequency detection works correctly", {
  # Test quarterly data
  quarterly_dates <- seq(as.Date("2000-01-01"), as.Date("2010-01-01"), by = "quarter")
  freq_q <- .detect_frequency(quarterly_dates, .quiet = TRUE)
  expect_equal(freq_q, 4)

  # Test monthly data
  monthly_dates <- seq(as.Date("2000-01-01"), as.Date("2005-01-01"), by = "month")
  freq_m <- .detect_frequency(monthly_dates, .quiet = TRUE)
  expect_equal(freq_m, 12)
})

test_that("frequency detection handles irregular dates", {
  # Test with some missing dates (quarterly pattern)
  dates <- seq(as.Date("2000-01-01"), as.Date("2010-01-01"), by = "quarter")
  irregular_dates <- dates[-c(5, 10, 15)]  # Remove some dates

  freq <- .detect_frequency(irregular_dates, .quiet = TRUE)
  expect_equal(freq, 4)
})

test_that("frequency detection fails appropriately", {
  # Too few dates
  expect_error(
    .detect_frequency(as.Date("2000-01-01"), .quiet = TRUE),
    "at least 2"
  )

  # Completely irregular dates
  irregular_dates <- as.Date(c("2000-01-01", "2000-02-15", "2000-05-20", "2000-12-31"))
  expect_error(
    .detect_frequency(irregular_dates, .quiet = TRUE),
    "Cannot auto-detect"
  )
})

test_that("df_to_ts_internal works correctly", {
  # Test with quarterly data
  result <- .df_to_ts_internal(gdp_brazil_qtr, "date", "value", 4)
  expect_s3_class(result, "ts")
  expect_equal(frequency(result), 4)

  # Test with monthly data
  result_monthly <- .df_to_ts_internal(ibcbr, "date", "value", 12)
  expect_s3_class(result_monthly, "ts")
  expect_equal(frequency(result_monthly), 12)
})

test_that("df_to_ts_internal handles missing values", {
  # Create data with missing values
  test_data <- gdp_brazil_qtr
  test_data$value[5:10] <- NA
  test_data$date[15] <- NA

  result <- .df_to_ts_internal(test_data, "date", "value", 4)
  expect_s3_class(result, "ts")
  expect_true(length(result) < nrow(test_data))  # Should be shorter due to removed NAs
})

test_that("trends_to_df works with single trend", {
  # Create a simple ts object
  test_ts <- ts(rnorm(20), frequency = 4, start = c(2000, 1))

  result <- .trends_to_df(test_ts, "date", NULL)
  expect_s3_class(result, "tbl_df")
  expect_true("trend" %in% names(result))
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
  data2 <- tibble::tibble(date = as.Date("2000-01-01") + 0:9, trend_hp = rnorm(10))

  result <- .safe_merge(data1, data2, "date")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("value", "trend_hp") %in% names(result)))
  expect_equal(nrow(result), 10)
})

test_that("safe_merge handles naming conflicts", {
  data1 <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:9,
    value = rnorm(10),
    trend_hp = rnorm(10)  # This will conflict
  )
  data2 <- tibble::tibble(date = as.Date("2000-01-01") + 0:9, trend_hp = rnorm(10))

  expect_message(
    result <- .safe_merge(data1, data2, "date"),
    "Renamed conflicting columns"
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