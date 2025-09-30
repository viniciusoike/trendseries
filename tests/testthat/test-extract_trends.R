test_that("extract_trends basic functionality works", {
  # Convert to ts object
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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

test_that("extract_trends one-sided HP filter works correctly", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test default (two-sided)
  hp_twosided <- extract_trends(ts_data, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_twosided, "ts")

  # Test one-sided with hp_onesided = TRUE
  hp_onesided <- extract_trends(
    ts_data,
    methods = "hp",
    params = list(hp_onesided = TRUE),
    .quiet = TRUE
  )
  expect_s3_class(hp_onesided, "ts")

  # Test one-sided with hp_onesided = FALSE explicitly
  hp_twosided_explicit <- extract_trends(
    ts_data,
    methods = "hp",
    params = list(hp_onesided = FALSE),
    .quiet = TRUE
  )
  expect_s3_class(hp_twosided_explicit, "ts")

  # Verify one-sided differs from two-sided (especially near endpoints)
  expect_false(identical(hp_onesided, hp_twosided))

  # One-sided and two-sided with explicit FALSE should be identical
  expect_equal(hp_twosided, hp_twosided_explicit)

  # Test with monthly data
  monthly_ts <- ts(rnorm(120), frequency = 12)
  hp_monthly_onesided <- extract_trends(
    monthly_ts,
    methods = "hp",
    params = list(hp_onesided = TRUE),
    .quiet = TRUE
  )
  expect_s3_class(hp_monthly_onesided, "ts")

  # Test custom lambda + one-sided combination
  hp_custom_onesided <- extract_trends(
    ts_data,
    methods = "hp",
    smoothing = 800,
    params = list(hp_onesided = TRUE),
    .quiet = TRUE
  )
  expect_s3_class(hp_custom_onesided, "ts")
  expect_false(identical(hp_custom_onesided, hp_onesided))
})

test_that("extract_trends moving average works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  stl_trend <- extract_trends(ts_data, methods = "stl", .quiet = TRUE)
  expect_s3_class(stl_trend, "ts")
})

test_that("extract_trends polynomial works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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
  data_df <- gdp_construction

  # Should convert via tsbox and work
  result <- extract_trends(data_df, methods = "hp", .quiet = TRUE)
  expect_s3_class(result, "ts")
})

test_that("extract_trends warns for short series", {
  # Create short ts
  short_ts <- ts(rnorm(10), frequency = 4)

  expect_warning(
    extract_trends(short_ts, methods = "hp"),
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
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

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

# Tests for new enhanced parameters

test_that("extract_trends spline cv parameter works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test with cv = NULL (no cross-validation, default)
  spline_no_cv <- extract_trends(
    ts_data,
    methods = "spline",
    params = list(spline_cv = NULL),
    .quiet = TRUE
  )
  expect_s3_class(spline_no_cv, "ts")

  # Test with cv = TRUE (leave-one-out CV)
  spline_loo_cv <- extract_trends(
    ts_data,
    methods = "spline",
    params = list(spline_cv = TRUE),
    .quiet = TRUE
  )
  expect_s3_class(spline_loo_cv, "ts")

  # Test with cv = FALSE (GCV)
  spline_gcv <- extract_trends(
    ts_data,
    methods = "spline",
    params = list(spline_cv = FALSE),
    .quiet = TRUE
  )
  expect_s3_class(spline_gcv, "ts")

  # Test with both spar and cv parameters
  spline_both <- extract_trends(
    ts_data,
    methods = "spline",
    smoothing = 0.5,
    params = list(spline_cv = FALSE),
    .quiet = TRUE
  )
  expect_s3_class(spline_both, "ts")
})

test_that("extract_trends polynomial raw parameter works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test with raw = FALSE (orthogonal, default)
  poly_orth <- extract_trends(
    ts_data,
    methods = "poly",
    params = list(poly_degree = 2, poly_raw = FALSE),
    .quiet = TRUE
  )
  expect_s3_class(poly_orth, "ts")

  # Test with raw = TRUE (raw polynomials)
  poly_raw <- extract_trends(
    ts_data,
    methods = "poly",
    params = list(poly_degree = 2, poly_raw = TRUE),
    .quiet = TRUE
  )
  expect_s3_class(poly_raw, "ts")

  # Results should be identical in fitted values (different coefficients)
  # but we don't test coefficient equality, just that both work
  expect_equal(length(poly_orth), length(poly_raw))
})

test_that("extract_trends polynomial degree warning works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test that degree > 3 generates a warning
  expect_warning(
    extract_trends(
      ts_data,
      methods = "poly",
      params = list(poly_degree = 5)
    ),
    "Polynomial degree > 3"
  )

  # But still produces output
  poly_high <- suppressWarnings(
    extract_trends(
      ts_data,
      methods = "poly",
      params = list(poly_degree = 5),
      .quiet = TRUE
    )
  )
  expect_s3_class(poly_high, "ts")

  # No warning for degree <= 3
  expect_no_warning(
    extract_trends(
      ts_data,
      methods = "poly",
      params = list(poly_degree = 3),
      .quiet = TRUE
    )
  )
})

test_that("extract_trends UCM type parameter works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test with type = "level" (default)
  ucm_level <- extract_trends(
    ts_data,
    methods = "ucm",
    params = list(ucm_type = "level"),
    .quiet = TRUE
  )
  expect_s3_class(ucm_level, "ts")
  expect_equal(length(ucm_level), length(ts_data))

  # Test with type = "trend"
  ucm_trend <- extract_trends(
    ts_data,
    methods = "ucm",
    params = list(ucm_type = "trend"),
    .quiet = TRUE
  )
  expect_s3_class(ucm_trend, "ts")
  expect_equal(length(ucm_trend), length(ts_data))

  # Test with type = "BSM" (requires seasonal data)
  ucm_bsm <- extract_trends(
    ts_data,
    methods = "ucm",
    params = list(ucm_type = "BSM"),
    .quiet = TRUE
  )
  expect_s3_class(ucm_bsm, "ts")
  expect_equal(length(ucm_bsm), length(ts_data))

  # All three types should work without error
  expect_true(all(!is.na(c(ucm_level, ucm_trend, ucm_bsm))))
})

test_that("extract_trends UCM validation works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test invalid type
  expect_error(
    extract_trends(
      ts_data,
      methods = "ucm",
      params = list(ucm_type = "invalid"),
      .quiet = TRUE
    ),
    "UCM type must be one of"
  )
})

test_that("enhanced parameters work with augment_trends", {
  # Test that new parameters also work with augment_trends
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)
  df_data <- ts_to_df(ts_data, date_col = "date", value_col = "value")

  # Test spline cv
  result_spline <- augment_trends(
    df_data,
    date_col = "date",
    value_col = "value",
    methods = "spline",
    params = list(spline_cv = TRUE),
    .quiet = TRUE
  )
  expect_true("trend_spline" %in% names(result_spline))

  # Test poly raw
  result_poly <- augment_trends(
    df_data,
    date_col = "date",
    value_col = "value",
    methods = "poly",
    params = list(poly_degree = 2, poly_raw = TRUE),
    .quiet = TRUE
  )
  expect_true("trend_poly" %in% names(result_poly))

  # Test ucm type
  result_ucm <- augment_trends(
    df_data,
    date_col = "date",
    value_col = "value",
    methods = "ucm",
    params = list(ucm_type = "trend"),
    .quiet = TRUE
  )
  expect_true("trend_ucm" %in% names(result_ucm))
})