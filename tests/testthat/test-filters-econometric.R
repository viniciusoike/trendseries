test_that("HP filter works correctly", {
  # Test with quarterly data
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test basic functionality
  hp_trend <- extract_trends(ts_data, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_trend, "ts")
  expect_equal(length(hp_trend), length(ts_data))
  expect_false(any(is.na(hp_trend)))

  # Test default lambda for quarterly data (should be 1600)
  hp_default <- extract_trends(ts_data, methods = "hp", .quiet = TRUE)
  hp_1600 <- extract_trends(ts_data, methods = "hp", smoothing = 1600, .quiet = TRUE)
  expect_equal(as.numeric(hp_default), as.numeric(hp_1600), tolerance = 1e-10)

  # Test monthly data default (should be 14400)
  ts_monthly <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)
  hp_monthly_default <- extract_trends(ts_monthly, methods = "hp", .quiet = TRUE)
  hp_monthly_14400 <- extract_trends(ts_monthly, methods = "hp", smoothing = 14400, .quiet = TRUE)
  expect_equal(as.numeric(hp_monthly_default), as.numeric(hp_monthly_14400), tolerance = 1e-10)
})

test_that("Baxter-King filter works correctly", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test basic functionality
  bk_trend <- extract_trends(ts_data, methods = "bk", .quiet = TRUE)
  expect_s3_class(bk_trend, "ts")
  expect_equal(length(bk_trend), length(ts_data))

  # Should have NAs at endpoints due to bandpass nature
  expect_true(any(is.na(bk_trend)))

  # Test custom band parameters
  bk_custom <- extract_trends(ts_data, methods = "bk", band = c(8, 40), .quiet = TRUE)
  expect_s3_class(bk_custom, "ts")
  expect_false(identical(as.numeric(bk_trend), as.numeric(bk_custom)))
})

test_that("Christiano-Fitzgerald filter works correctly", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test basic functionality
  cf_trend <- extract_trends(ts_data, methods = "cf", .quiet = TRUE)
  expect_s3_class(cf_trend, "ts")
  expect_equal(length(cf_trend), length(ts_data))

  # CF should handle endpoints better than BK (fewer NAs)
  bk_trend <- extract_trends(ts_data, methods = "bk", .quiet = TRUE)
  expect_true(sum(is.na(cf_trend)) <= sum(is.na(bk_trend)))
})

test_that("Hamilton filter works correctly", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test basic functionality
  hamilton_trend <- extract_trends(ts_data, methods = "hamilton", .quiet = TRUE)
  expect_s3_class(hamilton_trend, "ts")
  # Hamilton filter produces shorter series due to its regression nature
  expect_true(length(hamilton_trend) <= length(ts_data))

  # Test with short series (should fail)
  short_ts <- ts(1:10, frequency = 4)
  expect_error(
    extract_trends(short_ts, methods = "hamilton", .quiet = TRUE),
    "Time series too short"
  )

  # Test custom parameters
  hamilton_custom <- extract_trends(
    ts_data,
    methods = "hamilton",
    params = list(hamilton_h = 4, hamilton_p = 2),
    .quiet = TRUE
  )
  expect_s3_class(hamilton_custom, "ts")
})

test_that("Beveridge-Nelson decomposition works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test basic functionality
  bn_trend <- extract_trends(ts_data, methods = "bn", .quiet = TRUE)
  expect_s3_class(bn_trend, "ts")
  expect_equal(length(bn_trend), length(ts_data))
  # BN decomposition may have some NAs depending on AR model selection
  expect_true(sum(is.na(bn_trend)) < length(bn_trend) * 0.5)  # Less than half should be NA
})

test_that("Unobserved Components Model works", {
  ts_data <- df_to_ts(gdp_construction, value_col = "gdp_construction", frequency = 4)

  # Test basic functionality
  ucm_trend <- extract_trends(ts_data, methods = "ucm", .quiet = TRUE)
  expect_s3_class(ucm_trend, "ts")
  expect_equal(length(ucm_trend), length(ts_data))
  expect_false(any(is.na(ucm_trend)))
})