daily_prices <- function() {
  data.frame(
    date = as.Date(c(
      "2024-01-02",
      "2024-01-15",
      "2024-01-31",
      "2024-02-01",
      "2024-02-28"
    )),
    value = c(10, 12, 11, 13, 15)
  )
}

test_that("daily data aggregates to the last observed value of each month", {
  data <- daily_prices()
  data <- data[c(3, 5, 1, 4, 2), ]

  result <- aggregate_series(data, period = "month", summary = "last")

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result), c("date", "value"))
  expect_identical(result$date, as.Date(c("2024-01-01", "2024-02-01")))
  expect_equal(result$value, c(11, 15))
})

test_that("every summary statistic matches a hand calculation", {
  data <- daily_prices()
  aggregate_with <- function(summary) {
    aggregate_series(data, period = "month", summary = summary)$value
  }

  expect_equal(aggregate_with("first"), c(10, 13))
  expect_equal(aggregate_with("last"), c(11, 15))
  expect_equal(aggregate_with("mean"), c(11, 14))
  expect_equal(aggregate_with("sum"), c(33, 28))
  expect_equal(aggregate_with("max"), c(12, 15))
  expect_equal(aggregate_with("min"), c(10, 13))
})

test_that("monthly flows sum to quarters and years", {
  data <- data.frame(
    date = seq.Date(as.Date("2023-02-01"), by = "month", length.out = 24) - 1,
    value = 1:24
  )

  quarterly <- aggregate_series(data, period = "quarter", summary = "sum")
  annual <- aggregate_series(data, period = "year", summary = "sum")

  expect_identical(quarterly$date[1:2], as.Date(c("2023-01-01", "2023-04-01")))
  expect_equal(quarterly$value, c(6, 15, 24, 33, 42, 51, 60, 69))
  expect_identical(annual$date, as.Date(c("2023-01-01", "2024-01-01")))
  expect_equal(annual$value, c(78, 222))
})

test_that("groups never mix and keep their columns", {
  data <- data.frame(
    date = rep(as.Date(c("2024-01-10", "2024-01-20", "2024-02-10")), 2),
    asset = rep(c("b", "a"), each = 3),
    value = c(1, 2, 3, 10, 20, 30)
  )
  data <- data[c(4, 1, 5, 2, 6, 3), ]

  result <- aggregate_series(
    data,
    group_cols = "asset",
    period = "month",
    summary = "sum"
  )

  expect_identical(names(result), c("asset", "date", "value"))
  expect_identical(result$asset, c("a", "a", "b", "b"))
  expect_equal(result$value, c(30, 30, 3, 3))
})

test_that("several value columns are aggregated with the same summary", {
  data <- daily_prices()
  data$volume <- c(1, 2, 3, 4, 5)

  result <- aggregate_series(
    data,
    value_col = c("value", "volume"),
    summary = "sum"
  )

  expect_identical(names(result), c("date", "value", "volume"))
  expect_equal(result$volume, c(6, 9))
})

test_that("missing values propagate unless na_rm = TRUE", {
  data <- daily_prices()
  data$value[c(3, 4)] <- NA

  kept <- aggregate_series(data, summary = "last")
  removed <- aggregate_series(data, summary = "last", na_rm = TRUE)
  first_removed <- aggregate_series(data, summary = "first", na_rm = TRUE)
  summed <- aggregate_series(data, summary = "sum")

  expect_equal(kept$value, c(NA, 15))
  expect_equal(removed$value, c(12, 15))
  expect_equal(first_removed$value, c(10, 15))
  expect_equal(summed$value, c(NA_real_, NA_real_))
})

test_that("an all-missing period stays missing with na_rm = TRUE", {
  data <- daily_prices()
  data$value[1:3] <- NA

  for (summary in c("first", "last", "mean", "sum", "max", "min")) {
    result <- aggregate_series(data, summary = summary, na_rm = TRUE)
    expect_true(is.na(result$value[1]), info = summary)
    expect_false(is.na(result$value[2]), info = summary)
  }
})

test_that("invalid input raises informative errors", {
  data <- daily_prices()

  expect_snapshot(error = TRUE, {
    aggregate_series(data, period = "week")
    aggregate_series(data, summary = "median")
    aggregate_series(data, value_col = "price")
    aggregate_series(data, na_rm = NA)
  })

  duplicated <- rbind(data, data[1, ])
  expect_snapshot(error = TRUE, aggregate_series(duplicated))

  missing_date <- data
  missing_date$date[2] <- NA
  expect_snapshot(error = TRUE, aggregate_series(missing_date))
})

test_that("first and last use the period boundary rows despite interior NA", {
  data <- data.frame(
    date = as.Date("2024-01-01") + 0:2,
    value = c(1, NA, 3)
  )
  data <- data[c(3, 1, 2), ]

  expect_equal(aggregate_series(data, summary = "first")$value, 1)
  expect_equal(aggregate_series(data, summary = "last")$value, 3)
  expect_equal(aggregate_series(data, summary = "mean")$value, NA_real_)
})

test_that("several grouping columns and integer values are supported", {
  data <- data.frame(
    date = rep(as.Date(c("2024-01-10", "2024-01-20")), 4),
    country = rep(c("br", "br", "us", "us"), each = 2),
    sector = factor(rep(c("x", "y"), each = 2, times = 2)),
    value = 1:8
  )

  result <- aggregate_series(
    data,
    group_cols = c("country", "sector", "country"),
    summary = "sum"
  )

  expect_identical(names(result), c("country", "sector", "date", "value"))
  expect_s3_class(result$sector, "factor")
  expect_type(result$value, "double")
  expect_equal(result$value, c(3, 7, 11, 15))
})

test_that("a grouped tibble is aggregated like a plain data frame", {
  skip_if_not_installed("dplyr")
  data <- daily_prices()
  data$asset <- "a"
  expected <- aggregate_series(data, group_cols = "asset", summary = "sum")

  from_grouped <- aggregate_series(
    dplyr::group_by(data, asset),
    group_cols = "asset",
    summary = "sum"
  )
  expect_identical(from_grouped, expected)
})

test_that("a period no coarser than the data warns", {
  data <- data.frame(
    date = seq.Date(as.Date("2024-01-01"), by = "quarter", length.out = 4),
    value = 1:4
  )

  expect_snapshot(result <- aggregate_series(data, period = "month"))
  expect_equal(result$value, 1:4)
})

test_that("grouping columns cannot double as date or value columns", {
  data <- daily_prices()
  data$asset <- "a"

  expect_snapshot(error = TRUE, {
    aggregate_series(data, group_cols = "date")
    aggregate_series(
      data,
      value_col = "value",
      group_cols = c("asset", "value")
    )
  })
})
