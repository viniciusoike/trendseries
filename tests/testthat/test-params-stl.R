# Test for STL parameter handling via params argument
# This tests the fix for the bug where params = list(s.window = 21) was being ignored

test_that("STL params with dot notation are correctly processed", {
  # Test with electric data
  data("electric", package = "trendseries")

  # Using window parameter directly
  df1 <- electric |>
    augment_trends(
      value_col = "consumption",
      methods = "stl",
      window = 21
    )

  # Using params list with dot notation
  df2 <- electric |>
    augment_trends(
      value_col = "consumption",
      methods = "stl",
      params = list(s.window = 21)
    )

  # Using params list with underscore notation
  df3 <- electric |>
    augment_trends(
      value_col = "consumption",
      methods = "stl",
      params = list(stl_s_window = 21)
    )

  # All three should produce identical results
  expect_equal(df1$trend_stl, df2$trend_stl)
  expect_equal(df1$trend_stl, df3$trend_stl)
  expect_equal(df2$trend_stl, df3$trend_stl)
})

test_that("STL params with extract_trends work correctly", {
  # Create a simple time series
  ts_data <- ts(rnorm(120) + 1:120, frequency = 12)

  # Using window parameter
  trend1 <- extract_trends(
    ts_data,
    methods = "stl",
    window = 13
  )

  # Using params with dot notation
  trend2 <- extract_trends(
    ts_data,
    methods = "stl",
    params = list(s.window = 13)
  )

  # Using params with underscore notation
  trend3 <- extract_trends(
    ts_data,
    methods = "stl",
    params = list(stl_s_window = 13)
  )

  # All should be identical
  expect_equal(trend1, trend2)
  expect_equal(trend1, trend3)
  expect_equal(trend2, trend3)
})

test_that("STL robust parameter works via params", {
  ts_data <- ts(rnorm(120) + 1:120, frequency = 12)

  # Test robust parameter with dot notation
  trend_robust1 <- extract_trends(
    ts_data,
    methods = "stl",
    params = list(robust = TRUE)
  )

  # Test robust parameter with underscore notation
  trend_robust2 <- extract_trends(
    ts_data,
    methods = "stl",
    params = list(stl_robust = TRUE)
  )

  # Both notations should work
  expect_equal(trend_robust1, trend_robust2)
  expect_s3_class(trend_robust1, "ts")
})

test_that("STL t.window parameter works via params", {
  ts_data <- ts(rnorm(120) + 1:120, frequency = 12)

  # Test t.window parameter with dot notation
  trend1 <- extract_trends(
    ts_data,
    methods = "stl",
    params = list(s.window = 13, t.window = 25)
  )

  # Test t.window parameter with underscore notation
  trend2 <- extract_trends(
    ts_data,
    methods = "stl",
    params = list(stl_s_window = 13, stl_t_window = 25)
  )

  # Both should work
  expect_equal(trend1, trend2)
  expect_s3_class(trend1, "ts")
})

test_that("Multiple STL parameters work together via params", {
  ts_data <- ts(rnorm(120) + 1:120, frequency = 12)

  # Test all STL parameters together
  trend <- extract_trends(
    ts_data,
    methods = "stl",
    params = list(
      s.window = 13,
      t.window = 25,
      robust = TRUE
    )
  )

  expect_s3_class(trend, "ts")
  expect_equal(length(trend), length(ts_data))
})

test_that("params override unified window parameter for STL", {
  ts_data <- ts(rnorm(120) + 1:120, frequency = 12)

  # When both window and params are provided, params should take precedence
  trend_window <- extract_trends(
    ts_data,
    methods = "stl",
    window = 13
  )

  trend_params <- extract_trends(
    ts_data,
    methods = "stl",
    window = 99,  # This should be ignored
    params = list(s.window = 13)  # This should be used
  )

  # The params value should override the window value
  expect_equal(trend_window, trend_params)
})

test_that("Unrecognized STL parameters trigger warning", {
  ts_data <- ts(rnorm(120) + 1:120, frequency = 12)

  # Test that unrecognized parameters trigger a warning
  expect_warning(
    extract_trends(
      ts_data,
      methods = "stl",
      params = list(invalid_param = 123)
    ),
    "Unrecognized parameters"
  )
})

test_that("STL parameter normalization handles mixed notation", {
  ts_data <- ts(rnorm(120) + 1:120, frequency = 12)

  # Test mixing dot and underscore notation (shouldn't happen in practice, but should work)
  trend <- extract_trends(
    ts_data,
    methods = "stl",
    params = list(
      s.window = 13,  # dot notation
      stl_robust = TRUE  # underscore notation
    )
  )

  expect_s3_class(trend, "ts")
})

test_that("STL params work with grouped data in augment_trends", {
  # Create grouped test data
  test_data <- data.frame(
    date = rep(seq.Date(as.Date("2020-01-01"), by = "month", length.out = 60), 2),
    group = rep(c("A", "B"), each = 60),
    value = c(rnorm(60, 100, 10), rnorm(60, 200, 20))
  )

  # Test with grouped data using params
  result <- test_data |>
    augment_trends(
      date_col = "date",
      value_col = "value",
      group_vars = "group",
      methods = "stl",
      params = list(s.window = 13)
    )

  expect_true("trend_stl" %in% names(result))
  expect_equal(nrow(result), 120)

  # Compare with window parameter
  result2 <- test_data |>
    augment_trends(
      date_col = "date",
      value_col = "value",
      group_vars = "group",
      methods = "stl",
      window = 13
    )

  expect_equal(result$trend_stl, result2$trend_stl)
})
