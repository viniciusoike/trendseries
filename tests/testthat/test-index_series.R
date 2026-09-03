test_that("default indexing uses the earliest observed value and preserves rows", {
  data <- data.frame(
    id = c("c", "a", "b"),
    date = as.Date(c("2020-03-31", "2020-01-31", "2020-02-29")),
    value = c(30, NA, 20)
  )

  result <- index_series(data)

  expect_s3_class(result, "tbl_df")
  expect_identical(result$id, data$id)
  expect_identical(names(result), c(names(data), "index_value"))
  expect_equal(result$index_value, c(150, NA, 100))
})

test_that("explicit years and date ranges use period means", {
  data <- data.frame(
    date = seq.Date(as.Date("2019-01-01"), as.Date("2020-12-01"), by = "month"),
    value = 1:24
  )

  by_year <- index_series(data, base_period = 2019L)
  by_dates <- index_series(
    data,
    base_period = as.Date(c("2019-01-31", "2019-12-31"))
  )
  two_years <- index_series(data, base_period = c(2020L, 2019L))

  expect_equal(by_year$index_value, by_dates$index_value)
  expect_equal(mean(by_year$index_value[1:12]), 100)
  expect_equal(mean(two_years$index_value), 100)
})

test_that("date matching uses calendar-period resolution and inclusive bounds", {
  data <- data.frame(
    date = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31")),
    value = c(10, 20, 30)
  )

  one_month <- index_series(data, base_period = as.Date("2020-02-01"))
  interval <- index_series(
    data,
    base_period = as.Date(c("2020-03-15", "2020-02-15")),
    base_value = 50
  )

  expect_equal(one_month$index_value, c(50, 100, 150))
  expect_equal(interval$index_value, c(20, 40, 60))
})

test_that("groups and multiple values receive independent references", {
  data <- data.frame(
    group = rep(c("a", "b"), each = 2),
    date = rep(as.Date(c("2020-01-01", "2020-02-01")), 2),
    sales = c(10, 20, 100, 300),
    stock = c(5, 10, 20, 10)
  )

  result <- index_series(
    data,
    value_col = c("sales", "stock"),
    group_cols = "group",
    suffix = "base"
  )

  expect_equal(result$index_sales_base, c(100, 200, 100, 300))
  expect_equal(result$index_stock_base, c(100, 200, 100, 50))
})

test_that("missing group values receive their own reference", {
  data <- data.frame(
    group = c("a", "a", NA, NA),
    date = rep(as.Date(c("2020-01-01", "2020-02-01")), 2),
    value = c(10, 20, 5, 15)
  )

  result <- index_series(data, group_cols = "group")

  expect_equal(result$index_value, c(100, 200, 100, 300))
})

test_that("missing base observations follow na_rm", {
  data <- data.frame(
    date = seq.Date(as.Date("2019-01-01"), as.Date("2020-12-01"), by = "month"),
    value = c(1:5, NA, 7:24)
  )
  data$value[20] <- NA

  expect_error(index_series(data, base_period = 2019L), "missing values")
  result <- index_series(data, base_period = 2019L, na_rm = TRUE)

  expect_equal(mean(result$index_value[1:12], na.rm = TRUE), 100)
  expect_equal(result$index_value[18], 18 / mean(c(1:5, 7:12)) * 100)
  expect_identical(result$index_value[20], NA_real_)
})

test_that("quiet controls frequency-detection messages", {
  data <- data.frame(
    date = seq.Date(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "month"),
    value = seq_len(12)
  )

  expect_message(index_series(data, base_period = 2019), "Auto-detected")
  expect_no_message(index_series(data, base_period = 2019, .quiet = TRUE))
})

test_that("coverage warnings are issued once per group, not per value", {
  data <- data.frame(
    group = rep(c("a", "b"), each = 6),
    date = rep(
      seq.Date(as.Date("2019-07-01"), by = "month", length.out = 6),
      2
    ),
    value = seq_len(12),
    other = seq_len(12) * 2
  )
  warnings <- character()

  withCallingHandlers(
    index_series(
      data,
      value_col = c("value", "other"),
      group_cols = "group",
      base_period = 2019,
      .quiet = TRUE
    ),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 2)
})

test_that("default references must be observed and non-zero", {
  dates <- as.Date(c("2020-01-01", "2020-02-01"))

  expect_error(
    index_series(data.frame(date = dates, value = c(NA_real_, NA_real_))),
    "no observed reference"
  )
  expect_error(
    index_series(data.frame(date = dates, value = c(0, 1))),
    "finite and non-zero"
  )
})

test_that("incomplete and unusable bases are reported", {
  partial <- data.frame(
    date = seq.Date(as.Date("2019-07-01"), as.Date("2020-06-01"), by = "month"),
    value = seq_len(12)
  )
  empty <- transform(partial, value = NA_real_)
  zero <- transform(partial, value = c(rep(0, 6), 1:6))

  expect_warning(index_series(partial, base_period = 2019L), "incomplete")
  expect_error(index_series(partial, base_period = 2018L), "no observations")
  expect_error(
    suppressWarnings(index_series(empty, base_period = 2019L)),
    "only missing"
  )
  expect_error(
    suppressWarnings(index_series(zero, base_period = 2019L)),
    "finite and non-zero"
  )
})

test_that("duplicate dates suggest grouping and names are not overwritten", {
  duplicated <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-01")),
    value = 1:2
  )
  conflict <- data.frame(
    date = as.Date(c("2020-01-01", "2020-02-01")),
    value = 1:2,
    index_value = 3:4
  )

  expect_error(index_series(duplicated), "group_cols")
  expect_warning(result <- index_series(conflict), "already exists")
  expect_identical(result$index_value, conflict$index_value)
  expect_equal(result$index_value_1, c(100, 200))
})

test_that("invalid arguments are rejected", {
  data <- data.frame(date = as.Date(c("2020-01-01", "2020-02-01")), value = 1:2)

  expect_error(index_series(data, date_col = "missing"), "not found")
  expect_error(index_series(data, value_col = character()), "non-empty")
  expect_error(index_series(data, group_cols = "missing"), "not found")
  expect_error(index_series(data, base_period = 2020.5), "four-digit integers")
  expect_error(index_series(data, base_value = 0), "positive")
  expect_error(index_series(data, na_rm = NA), "non-missing logical")
  expect_error(
    index_series(data, suffix = NA_character_),
    "non-missing character"
  )
  expect_error(
    index_series(data[1, ], base_period = 2020),
    "at least two dated observations"
  )
})
