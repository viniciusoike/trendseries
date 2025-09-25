test_that("Simple Moving Average works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  ma_trend <- extract_trends(ts_data, methods = "ma", .quiet = TRUE)
  expect_s3_class(ma_trend, "ts")
  expect_equal(length(ma_trend), length(ts_data))

  # MA should have NAs at the beginning (proper behavior for moving averages)
  # Default window is 12 for monthly data, so first 11 values should be NA
  expect_true(any(is.na(ma_trend)))
  expect_equal(sum(is.na(ma_trend)), 11)

  # Test custom window
  ma_custom <- extract_trends(ts_data, methods = "ma", window = 6, .quiet = TRUE)
  expect_s3_class(ma_custom, "ts")
  # Should have 5 NAs at beginning for window=6
  expect_equal(sum(is.na(ma_custom)), 5)
  # Non-NA portions should differ between window=12 and window=6
  expect_false(identical(as.numeric(ma_trend[!is.na(ma_trend)]),
                        as.numeric(ma_custom[!is.na(ma_custom)])))
})

test_that("EWMA works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  ewma_trend <- extract_trends(ts_data, methods = "ewma", .quiet = TRUE)
  expect_s3_class(ewma_trend, "ts")
  expect_equal(length(ewma_trend), length(ts_data))

  # EWMA may have some NAs at the beginning depending on implementation
  # TTR::EMA typically has n-1 NAs at the start
  # Just check that not all values are NA
  expect_false(all(is.na(ewma_trend)))

  # Test custom alpha
  ewma_custom <- extract_trends(ts_data, methods = "ewma", smoothing = 0.3, .quiet = TRUE)
  expect_s3_class(ewma_custom, "ts")
  expect_false(identical(as.numeric(ewma_trend), as.numeric(ewma_custom)))
})

test_that("DEMA works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  dema_trend <- extract_trends(ts_data, methods = "dema", .quiet = TRUE)
  expect_s3_class(dema_trend, "ts")
  expect_equal(length(dema_trend), length(ts_data))

  # Test custom period
  dema_custom <- extract_trends(ts_data, methods = "dema", window = 21, .quiet = TRUE)
  expect_s3_class(dema_custom, "ts")
  expect_false(identical(as.numeric(dema_trend), as.numeric(dema_custom)))
})

test_that("HMA works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  hma_trend <- extract_trends(ts_data, methods = "hma", .quiet = TRUE)
  expect_s3_class(hma_trend, "ts")
  expect_equal(length(hma_trend), length(ts_data))

  # Test custom period
  hma_custom <- extract_trends(ts_data, methods = "hma", window = 21, .quiet = TRUE)
  expect_s3_class(hma_custom, "ts")
  expect_false(identical(as.numeric(hma_trend), as.numeric(hma_custom)))
})

test_that("ALMA works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  alma_trend <- extract_trends(ts_data, methods = "alma", .quiet = TRUE)
  expect_s3_class(alma_trend, "ts")
  expect_equal(length(alma_trend), length(ts_data))

  # Test custom parameters
  alma_custom <- extract_trends(
    ts_data,
    methods = "alma",
    window = 14,
    params = list(alma_offset = 0.9, alma_sigma = 8),
    .quiet = TRUE
  )
  expect_s3_class(alma_custom, "ts")
  expect_false(identical(as.numeric(alma_trend), as.numeric(alma_custom)))
})

test_that("Multiple MA methods work together", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test multiple methods
  ma_trends <- extract_trends(
    ts_data,
    methods = c("ma", "ewma", "dema", "hma"),
    .quiet = TRUE
  )

  expect_type(ma_trends, "list")
  expect_equal(length(ma_trends), 4)
  expect_true(all(c("ma", "ewma", "dema", "hma") %in% names(ma_trends)))

  # All should be ts objects
  for (trend in ma_trends) {
    expect_s3_class(trend, "ts")
    expect_equal(length(trend), length(ts_data))
  }
})