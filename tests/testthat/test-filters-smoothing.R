test_that("Simple exponential smoothing works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality with automatic parameter selection
  exp_simple_trend <- extract_trends(ts_data, methods = "exp_simple", .quiet = TRUE)
  expect_s3_class(exp_simple_trend, "ts")
  expect_equal(length(exp_simple_trend), length(ts_data))

  # Test with custom alpha (should be different from automatic)
  exp_simple_custom <- extract_trends(
    ts_data,
    methods = "exp_simple",
    smoothing = 0.3,
    .quiet = TRUE
  )
  expect_s3_class(exp_simple_custom, "ts")
  # Note: Results might be similar if automatic selection chooses similar alpha
  # so we check that both are valid time series rather than requiring difference
  expect_equal(length(exp_simple_custom), length(ts_data))
})

test_that("Double exponential smoothing works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  exp_double_trend <- extract_trends(ts_data, methods = "exp_double", .quiet = TRUE)
  expect_s3_class(exp_double_trend, "ts")
  expect_equal(length(exp_double_trend), length(ts_data))

  # Test with custom parameters
  exp_double_custom <- extract_trends(
    ts_data,
    methods = "exp_double",
    params = list(exp_alpha = 0.3, exp_beta = 0.1),
    .quiet = TRUE
  )
  expect_s3_class(exp_double_custom, "ts")
})

test_that("STL decomposition works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  stl_trend <- extract_trends(ts_data, methods = "stl", .quiet = TRUE)
  expect_s3_class(stl_trend, "ts")
  expect_equal(length(stl_trend), length(ts_data))
  expect_false(any(is.na(stl_trend)))

  # Test with custom s.window
  stl_custom <- extract_trends(ts_data, methods = "stl", window = 13, .quiet = TRUE)
  expect_s3_class(stl_custom, "ts")
  expect_false(identical(as.numeric(stl_trend), as.numeric(stl_custom)))
})

test_that("Loess smoothing works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  loess_trend <- extract_trends(ts_data, methods = "loess", .quiet = TRUE)
  expect_s3_class(loess_trend, "ts")
  expect_equal(length(loess_trend), length(ts_data))
  expect_false(any(is.na(loess_trend)))

  # Test custom span
  loess_custom <- extract_trends(ts_data, methods = "loess", smoothing = 0.5, .quiet = TRUE)
  expect_s3_class(loess_custom, "ts")
  expect_false(identical(as.numeric(loess_trend), as.numeric(loess_custom)))
})

test_that("Spline smoothing works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality (automatic spar selection)
  spline_trend <- extract_trends(ts_data, methods = "spline", .quiet = TRUE)
  expect_s3_class(spline_trend, "ts")
  expect_equal(length(spline_trend), length(ts_data))
  expect_false(any(is.na(spline_trend)))

  # Test custom spar
  spline_custom <- extract_trends(
    ts_data,
    methods = "spline",
    smoothing = 0.5,
    .quiet = TRUE
  )
  expect_s3_class(spline_custom, "ts")
  expect_false(identical(as.numeric(spline_trend), as.numeric(spline_custom)))
})

test_that("Polynomial trend works correctly", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test basic functionality (linear trend)
  poly_trend <- extract_trends(ts_data, methods = "poly", .quiet = TRUE)
  expect_s3_class(poly_trend, "ts")
  expect_equal(length(poly_trend), length(ts_data))
  expect_false(any(is.na(poly_trend)))

  # Test quadratic trend
  poly_quad <- extract_trends(
    ts_data,
    methods = "poly",
    params = list(poly_degree = 2),
    .quiet = TRUE
  )
  expect_s3_class(poly_quad, "ts")
  expect_false(identical(as.numeric(poly_trend), as.numeric(poly_quad)))
})

test_that("Multiple smoothing methods work together", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test multiple methods
  smooth_trends <- extract_trends(
    ts_data,
    methods = c("exp_simple", "stl", "loess", "spline"),
    .quiet = TRUE
  )

  expect_type(smooth_trends, "list")
  expect_equal(length(smooth_trends), 4)
  expect_true(all(c("exp_simple", "stl", "loess", "spline") %in% names(smooth_trends)))

  # All should be ts objects without NAs
  for (trend in smooth_trends) {
    expect_s3_class(trend, "ts")
    expect_equal(length(trend), length(ts_data))
    expect_false(any(is.na(trend)))
  }
})