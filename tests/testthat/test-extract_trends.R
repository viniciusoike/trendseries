test_that("extract_trends basic functionality works", {
  # Convert to ts object
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  # Test single method
  hp_trend <- extract_trends(ts_data, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_trend, "ts")
  expect_equal(length(hp_trend), length(ts_data))

  # Test multiple methods
  all_trends <- extract_trends(
    ts_data,
    methods = c("hp", "ma", "poly"),
    .quiet = TRUE
  )
  expect_type(all_trends, "list")
  expect_equal(length(all_trends), 3)
  expect_true(all(c("hp", "ma", "poly") %in% names(all_trends)))
})

test_that("extract_trends validates frequency", {
  # Create daily frequency ts (should fail)
  daily_ts <- ts(rnorm(100), frequency = 365)

  expect_error(
    extract_trends(daily_ts),
    "Only monthly.*quarterly.*frequencies"
  )
})

test_that("extract_trends HP filter works correctly", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  # Test with default lambda
  hp_trend <- extract_trends(ts_data, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_trend, "ts")

  # Test with custom lambda using unified parameter
  hp_custom <- extract_trends(
    ts_data,
    methods = "hp",
    smoothing = 1000,
    .quiet = TRUE
  )
  expect_s3_class(hp_custom, "ts")
  expect_false(identical(hp_trend, hp_custom))
})

test_that("extract_trends moving average works", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  ma_trend <- extract_trends(ts_data, methods = "ma", .quiet = TRUE)
  expect_s3_class(ma_trend, "ts")

  # Test custom window using unified parameter
  ma_custom <- extract_trends(
    ts_data,
    methods = "ma",
    window = 8,
    .quiet = TRUE
  )
  expect_s3_class(ma_custom, "ts")
})

test_that("extract_trends STL works", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  stl_trend <- extract_trends(ts_data, methods = "stl", .quiet = TRUE)
  expect_s3_class(stl_trend, "ts")
})

test_that("extract_trends polynomial works", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  poly_trend <- extract_trends(ts_data, methods = "poly", .quiet = TRUE)
  expect_s3_class(poly_trend, "ts")

  # Test different degrees using params
  poly_quad <- extract_trends(
    ts_data,
    methods = "poly",
    params = list(poly_degree = 2),
    .quiet = TRUE
  )
  expect_s3_class(poly_quad, "ts")
})

test_that("extract_trends loess works", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  loess_trend <- extract_trends(ts_data, methods = "loess", .quiet = TRUE)
  expect_s3_class(loess_trend, "ts")

  # Test custom span using unified parameter
  loess_custom <- extract_trends(
    ts_data,
    methods = "loess",
    smoothing = 0.5,
    .quiet = TRUE
  )
  expect_s3_class(loess_custom, "ts")
})

test_that("extract_trends spline works", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  spline_trend <- extract_trends(ts_data, methods = "spline", .quiet = TRUE)
  expect_s3_class(spline_trend, "ts")

  # Test custom spar using unified parameter
  spline_custom <- extract_trends(
    ts_data,
    methods = "spline",
    smoothing = 0.5,
    .quiet = TRUE
  )
  expect_s3_class(spline_custom, "ts")
})

test_that("extract_trends handles non-ts input via tsbox", {
  # This tests the tsbox integration
  data_df <- gdp_brazil_qtr

  # Should convert via tsbox and work
  result <- extract_trends(data_df, methods = "hp", .quiet = TRUE)
  expect_s3_class(result, "ts")
})

test_that("extract_trends warns for short series", {
  # Create short ts
  short_ts <- ts(rnorm(10), frequency = 4)

  expect_warning(
    extract_trends(short_ts, methods = "hp", .quiet = TRUE),
    "observations"
  )
})

test_that("extract_trends economic defaults work correctly", {
  # Quarterly data should use lambda = 1600
  quarterly_ts <- ts(rnorm(40), frequency = 4)
  hp_quarterly <- extract_trends(quarterly_ts, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_quarterly, "ts")

  # Monthly data should use lambda = 14400
  monthly_ts <- ts(rnorm(120), frequency = 12)
  hp_monthly <- extract_trends(monthly_ts, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_monthly, "ts")
})

test_that("extract_trends validates methods", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  expect_error(
    extract_trends(ts_data, methods = "invalid_method"),
    "Invalid methods"
  )

  expect_error(
    extract_trends(ts_data, methods = c("hp", "invalid", "ma")),
    "Invalid methods"
  )
})

# Skip these tests if mFilter is not available
skip_if_not_installed("mFilter")

test_that("extract_trends Baxter-King filter works", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  bk_trend <- extract_trends(ts_data, methods = "bk", .quiet = TRUE)
  expect_s3_class(bk_trend, "ts")

  # Test custom bounds using unified parameter
  bk_custom <- extract_trends(
    ts_data,
    methods = "bk",
    band = c(8, 40),
    .quiet = TRUE
  )
  expect_s3_class(bk_custom, "ts")
})

test_that("extract_trends Christiano-Fitzgerald filter works", {
  ts_data <- df_to_ts(gdp_brazil_qtr, frequency = 4)

  cf_trend <- extract_trends(ts_data, methods = "cf", .quiet = TRUE)
  expect_s3_class(cf_trend, "ts")

  # Test custom bounds using unified parameter
  cf_custom <- extract_trends(
    ts_data,
    methods = "cf",
    band = c(8, 40),
    .quiet = TRUE
  )
  expect_s3_class(cf_custom, "ts")
})