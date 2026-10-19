make_monthly_gaps <- function(values, missing_rows = integer()) {
  data <- data.frame(
    date = seq(
      as.Date("2020-01-01"),
      by = "month",
      length.out = length(values)
    ),
    value = values
  )
  if (length(missing_rows) == 0) {
    return(data)
  }
  data[-missing_rows, , drop = FALSE]
}

test_that("interpolate_series completes the grid and preserves edge NAs", {
  data <- make_monthly_gaps(c(NA, 1, 2, NA, 5, NA), missing_rows = 3)

  result <- interpolate_series(
    data,
    frequency = 12,
    method = "linear",
    imputed_col = TRUE
  )

  expect_equal(
    result$date,
    seq(as.Date("2020-01-01"), by = "month", length.out = 6)
  )
  expect_equal(result$value, c(NA, 1, 7 / 3, 11 / 3, 5, NA))
  expect_equal(result$imputed_value, c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("frequency detection sees through an omitted monthly period", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-04-01")),
    value = c(1, 2, 4)
  )

  result <- interpolate_series(data, method = "linear", .quiet = TRUE)

  expect_equal(result$value, 1:4)
})

test_that("interpolate_series preserves consistent date conventions", {
  starts <- seq(as.Date("2020-01-01"), by = "month", length.out = 5)
  ends <- lubridate::ceiling_date(starts, "month") - 1
  middle <- starts + 14

  end_data <- data.frame(date = ends[-3], value = c(1, 2, 4, 5))
  middle_data <- data.frame(date = middle[-3], value = c(1, 2, 4, 5))

  end_result <- interpolate_series(end_data, frequency = 12, method = "linear")
  middle_result <- interpolate_series(
    middle_data,
    frequency = 12,
    method = "linear"
  )

  expect_equal(end_result$date, ends)
  expect_equal(middle_result$date, middle)
})

test_that("inconsistent dates are normalized to period starts", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2020-02-15", "2020-04-01")),
    value = c(1, 2, 4)
  )

  expect_snapshot(
    result <- interpolate_series(data, frequency = 12, method = "linear")
  )
  expect_equal(
    result$date,
    seq(as.Date("2020-01-01"), by = "month", length.out = 4)
  )
})

test_that("grouped multiple series are completed independently", {
  data <- data.frame(
    group = rep(c("a", "b"), each = 3),
    date = rep(as.Date(c("2020-01-01", "2020-02-01", "2020-04-01")), 2),
    x = c(1, 2, 4, 10, 20, 40),
    y = c(2, 4, 8, 5, 10, 20),
    note = letters[1:6]
  )

  result <- interpolate_series(
    data,
    value_col = c("x", "y"),
    group_cols = "group",
    frequency = 12,
    method = "linear",
    imputed_col = TRUE
  )

  expect_equal(nrow(result), 8)
  expect_equal(result$group, rep(c("a", "b"), each = 4))
  expect_equal(result$x, c(1, 2, 3, 4, 10, 20, 30, 40))
  expect_equal(result$y, c(2, 4, 6, 8, 5, 10, 15, 20))
  expect_equal(result$imputed_x, result$imputed_y)
  expect_equal(which(result$imputed_x), c(3, 7))
  expect_equal(result$note[c(3, 7)], c(NA_character_, NA_character_))
})

test_that("group combinations that print alike remain separate", {
  data <- data.frame(
    g1 = rep(c("a.b", "a"), each = 2),
    g2 = rep(c("c", "b.c"), each = 2),
    date = rep(as.Date(c("2020-01-01", "2020-03-01")), 2),
    value = c(1, 3, 10, 30)
  )

  result <- interpolate_series(
    data,
    group_cols = c("g1", "g2"),
    frequency = 12,
    method = "linear"
  )

  expect_equal(nrow(result), 6)
  expect_equal(result$value, c(10, 20, 30, 1, 2, 3))
})

test_that("constant series use a constant interior fill", {
  data <- make_monthly_gaps(c(4, 4, NA, 4, 4))

  result <- interpolate_series(data, frequency = 12, method = "kalman")

  expect_equal(result$value, rep(4, 5))
})

test_that("Kalman interpolation changes only missing observations", {
  set.seed(1)
  dates <- seq(as.Date("2018-01-01"), by = "month", length.out = 36)
  truth <- 100 + seq_along(dates) + 5 * sin(2 * pi * seq_along(dates) / 12)
  values <- truth + stats::rnorm(length(dates))
  gaps <- c(8, 9, 21)
  values[gaps] <- NA
  data <- data.frame(date = dates, value = values)

  result <- interpolate_series(data, frequency = 12, method = "kalman")

  expect_equal(result$value[-gaps], values[-gaps])
  expect_equal(all(is.finite(result$value)), TRUE)
  expect_lt(max(abs(result$value[gaps] - truth[gaps])), 3)
})

test_that("Kalman non-convergence warns and keeps the Kalman estimates", {
  fit_structts <- .fit_structts
  testthat::local_mocked_bindings(
    .fit_structts = function(...) {
      fit <- fit_structts(...)
      fit$code <- 52L
      fit
    },
    .package = "trendseries"
  )
  data <- make_monthly_gaps(c(1, 2, NA, 4, 5, NA, 7))

  for (method in c("auto", "kalman")) {
    expect_snapshot(
      result <- interpolate_series(data, frequency = 12, method = method)
    )
    expect_equal(all(is.finite(result$value)), TRUE)
  }
  expect_no_warning(
    quiet <- interpolate_series(
      data,
      frequency = 12,
      method = "kalman",
      .quiet = TRUE
    )
  )
  expect_equal(quiet$value, result$value)
})

test_that("auto falls back to linear when Kalman interpolation fails", {
  testthat::local_mocked_bindings(
    .kalman_interpolation = function(...) stop("forced Kalman failure"),
    .package = "trendseries"
  )
  data <- make_monthly_gaps(c(1, NA, 3))

  expect_snapshot(
    result <- interpolate_series(data, frequency = 12, method = "auto")
  )
  expect_equal(result$value, 1:3)

  expect_snapshot(
    error = TRUE,
    interpolate_series(data, frequency = 12, method = "kalman")
  )
})

test_that("spline interpolation uses the completed period positions", {
  data <- make_monthly_gaps(c(1, 4, NA, 16, 25))

  result <- interpolate_series(data, frequency = 12, method = "spline")

  expected <- stats::spline(
    x = c(1, 2, 4, 5),
    y = c(1, 4, 16, 25),
    xout = 3,
    method = "fmm"
  )$y
  expect_equal(result$value[3], expected)
})

test_that("the automatic maximum gap is enforced", {
  data <- make_monthly_gaps(c(1, rep(NA, 8), 10))

  expect_snapshot(
    error = TRUE,
    interpolate_series(data, frequency = 12, method = "linear")
  )

  expect_snapshot(
    result <- interpolate_series(
      data,
      frequency = 12,
      method = "linear",
      max_gap = Inf
    )
  )
  expect_equal(which(is.na(result$value)), integer())
})

test_that("duplicate periods and insufficient observations are rejected", {
  duplicates <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-31", "2020-02-01")),
    value = 1:3
  )
  sparse <- make_monthly_gaps(c(NA, 1, NA))

  expect_snapshot(
    error = TRUE,
    interpolate_series(duplicates, frequency = 12, method = "linear")
  )
  expect_snapshot(
    error = TRUE,
    interpolate_series(sparse, frequency = 12, method = "linear")
  )
})

test_that("imputation flag conflicts are rejected", {
  data <- make_monthly_gaps(c(1, NA, 3))
  data$imputed_value <- FALSE

  expect_snapshot(
    error = TRUE,
    interpolate_series(
      data,
      frequency = 12,
      method = "linear",
      imputed_col = TRUE
    )
  )
})

test_that("weekly grids preserve their weekday", {
  data <- data.frame(
    date = as.Date(c("2020-01-08", "2020-01-15", "2020-01-29")),
    value = c(1, 2, 4)
  )

  result <- interpolate_series(data, frequency = 52, method = "linear")

  expect_equal(
    result$date,
    as.Date(c("2020-01-08", "2020-01-15", "2020-01-22", "2020-01-29"))
  )
  expect_equal(result$value, 1:4)
})

test_that("completed output flows into downstream trend methods", {
  dates <- seq(as.Date("2018-01-01"), by = "month", length.out = 48)
  data <- data.frame(date = dates, value = seq_along(dates))
  data$value[20] <- NA
  data <- data[-10, ]

  completed <- interpolate_series(data, frequency = 12, method = "linear")
  result <- augment_trends(
    completed,
    frequency = 12,
    methods = "hp",
    .quiet = TRUE
  )

  expect_equal(result$date, dates)
  expect_equal(nrow(result), 48)
  expect_equal(anyNA(result$trend_hp), FALSE)
})

test_that("Kalman interpolation is accurate near the start of a trending series", {
  set.seed(1)
  dates <- seq(as.Date("2018-01-01"), by = "month", length.out = 36)
  t <- seq_along(dates)
  truth <- 100 + 3 * t + 5 * sin(2 * pi * t / 12)
  values <- truth + stats::rnorm(length(dates))
  gaps <- c(2, 3)
  values[gaps] <- NA
  data <- data.frame(date = dates, value = values)

  result <- interpolate_series(data, frequency = 12, method = "kalman")

  expect_lt(max(abs(result$value[gaps] - truth[gaps])), 3)
})

test_that("Kalman interpolation uses a trend model above monthly frequency", {
  types <- character()
  fit_structts <- .fit_structts
  testthat::local_mocked_bindings(
    .fit_structts = function(ts_data, type) {
      types <<- c(types, type)
      fit_structts(ts_data, type)
    },
    .package = "trendseries"
  )
  set.seed(1)
  dates <- seq(as.Date("2020-01-06"), by = "week", length.out = 156)
  values <- 100 + seq_along(dates) / 10 + stats::rnorm(length(dates))
  values[50] <- NA
  data <- data.frame(date = dates, value = values)

  interpolate_series(data, frequency = 52, method = "kalman", .quiet = TRUE)

  expect_equal(types, "trend")
})

test_that("reconstructed calendar dates stay inside short months", {
  data <- data.frame(
    date = as.Date(c("2020-12-30", "2021-01-30", "2021-03-30", "2021-04-30")),
    value = c(1, 2, 4, 5)
  )

  result <- interpolate_series(data, frequency = 12, method = "linear")

  expect_equal(
    result$date,
    as.Date(c(
      "2020-12-30",
      "2021-01-30",
      "2021-02-28",
      "2021-03-30",
      "2021-04-30"
    ))
  )
  expect_equal(result$value, c(1, 2, 3, 4, 5))
})

test_that("frequency detection requires groups to agree", {
  monthly <- seq(as.Date("2020-01-01"), by = "month", length.out = 6)
  quarterly <- seq(as.Date("2020-01-01"), by = "quarter", length.out = 6)
  data <- data.frame(
    id = rep(c("a", "b"), each = 6),
    date = c(quarterly, monthly),
    value = c(1:6, 1:6)
  )

  expect_snapshot(
    error = TRUE,
    interpolate_series(data, group_cols = "id", method = "linear")
  )

  agree <- data
  agree$date <- c(monthly, monthly)
  expect_message(
    interpolate_series(agree, group_cols = "id", method = "linear"),
    "frequency 12"
  )
})
