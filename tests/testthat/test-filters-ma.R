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


test_that("Multiple MA methods work together", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test multiple methods
  ma_trends <- extract_trends(
    ts_data,
    methods = c("ma", "ewma", "wma", "zlema"),
    .quiet = TRUE
  )

  expect_type(ma_trends, "list")
  expect_equal(length(ma_trends), 4)
  expect_true(all(c("ma", "ewma", "wma", "zlema") %in% names(ma_trends)))

  # All should be ts objects
  for (trend in ma_trends) {
    expect_s3_class(trend, "ts")
    expect_equal(length(trend), length(ts_data))
  }
})

test_that("WMA works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality with default linear weights
  wma_trend <- extract_trends(ts_data, methods = "wma", .quiet = TRUE)
  expect_s3_class(wma_trend, "ts")
  expect_equal(length(wma_trend), length(ts_data))

  # WMA should have NAs at the beginning
  # Default window is 12 for monthly data, so first 11 values should be NA
  expect_true(any(is.na(wma_trend)))
  expect_equal(sum(is.na(wma_trend)), 11)

  # Test custom window
  wma_custom <- extract_trends(ts_data, methods = "wma", window = 6, .quiet = TRUE)
  expect_s3_class(wma_custom, "ts")
  expect_equal(sum(is.na(wma_custom)), 5)

  # Test custom weights via params
  custom_weights <- c(1, 2, 3, 4, 5)
  wma_weights <- extract_trends(
    ts_data,
    methods = "wma",
    window = 5,
    params = list(wma_weights = custom_weights),
    .quiet = TRUE
  )
  expect_s3_class(wma_weights, "ts")
  expect_equal(sum(is.na(wma_weights)), 4)
})

test_that("ZLEMA works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  zlema_trend <- extract_trends(ts_data, methods = "zlema", .quiet = TRUE)
  expect_s3_class(zlema_trend, "ts")
  expect_equal(length(zlema_trend), length(ts_data))

  # ZLEMA should have some NAs at the beginning
  expect_true(any(is.na(zlema_trend)))

  # Test custom window
  zlema_custom <- extract_trends(ts_data, methods = "zlema", window = 8, .quiet = TRUE)
  expect_s3_class(zlema_custom, "ts")

  # Test custom ratio via params
  zlema_ratio <- extract_trends(
    ts_data,
    methods = "zlema",
    window = 10,
    params = list(zlema_ratio = 0.5),
    .quiet = TRUE
  )
  expect_s3_class(zlema_ratio, "ts")
})

test_that("Triangular MA works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality with center alignment
  triangular_trend <- extract_trends(ts_data, methods = "triangular", .quiet = TRUE)
  expect_s3_class(triangular_trend, "ts")
  expect_equal(length(triangular_trend), length(ts_data))

  # Triangular MA should have NAs
  expect_true(any(is.na(triangular_trend)))

  # Test custom window
  triangular_custom <- extract_trends(ts_data, methods = "triangular", window = 7, .quiet = TRUE)
  expect_s3_class(triangular_custom, "ts")

  # Test right alignment using unified align parameter
  triangular_right <- extract_trends(
    ts_data,
    methods = "triangular",
    window = 5,
    align = "right",
    .quiet = TRUE
  )
  expect_s3_class(triangular_right, "ts")

  # Test center alignment using unified align parameter (explicit)
  triangular_center <- extract_trends(
    ts_data,
    methods = "triangular",
    window = 5,
    align = "center",
    .quiet = TRUE
  )
  expect_s3_class(triangular_center, "ts")

  # Right and center should have different patterns of NAs
  expect_false(identical(is.na(triangular_right), is.na(triangular_center)))
})

test_that("New methods work with multiple methods call", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test multiple new methods together
  new_ma_trends <- extract_trends(
    ts_data,
    methods = c("wma", "zlema", "triangular"),
    window = 6,
    .quiet = TRUE
  )

  expect_type(new_ma_trends, "list")
  expect_equal(length(new_ma_trends), 3)
  expect_true(all(c("wma", "zlema", "triangular") %in% names(new_ma_trends)))

  # All should be ts objects
  for (trend in new_ma_trends) {
    expect_s3_class(trend, "ts")
    expect_equal(length(trend), length(ts_data))
  }
})

test_that("New methods handle edge cases correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test minimum window sizes
  expect_error(
    extract_trends(ts_data, methods = "wma", window = 1, .quiet = TRUE),
    "WMA window must be at least 2"
  )

  expect_error(
    extract_trends(ts_data, methods = "triangular", window = 2, .quiet = TRUE),
    "Triangular MA window must be at least 3"
  )

  # Test invalid parameters
  expect_error(
    extract_trends(
      ts_data,
      methods = "triangular",
      params = list(triangular_align = "invalid"),
      .quiet = TRUE
    ),
    "align must be 'center' or 'right'"
  )

  expect_error(
    extract_trends(
      ts_data,
      methods = "zlema",
      params = list(zlema_ratio = 1.5),
      .quiet = TRUE
    ),
    "ratio must be a single numeric value between 0 and 1"
  )
})

test_that("Median filter works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  median_trend <- extract_trends(ts_data, methods = "median", .quiet = TRUE)
  expect_s3_class(median_trend, "ts")
  expect_equal(length(median_trend), length(ts_data))

  # Test custom window
  median_custom <- extract_trends(ts_data, methods = "median", window = 7, .quiet = TRUE)
  expect_s3_class(median_custom, "ts")
  expect_false(identical(as.numeric(median_trend), as.numeric(median_custom)))

  # Test custom endrule via params
  median_endrule <- extract_trends(
    ts_data,
    methods = "median",
    window = 5,
    params = list(median_endrule = "constant"),
    .quiet = TRUE
  )
  expect_s3_class(median_endrule, "ts")

  # Test invalid window (even number)
  expect_error(
    extract_trends(ts_data, methods = "median", window = 4, .quiet = TRUE),
    "Median filter window must be odd"
  )

  # Test invalid endrule
  expect_error(
    extract_trends(
      ts_data,
      methods = "median",
      params = list(median_endrule = "invalid"),
      .quiet = TRUE
    ),
    "endrule must be one of"
  )
})

test_that("Gaussian filter works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  gaussian_trend <- extract_trends(ts_data, methods = "gaussian", .quiet = TRUE)
  expect_s3_class(gaussian_trend, "ts")
  expect_equal(length(gaussian_trend), length(ts_data))

  # Test custom window
  gaussian_custom <- extract_trends(ts_data, methods = "gaussian", window = 9, .quiet = TRUE)
  expect_s3_class(gaussian_custom, "ts")
  expect_false(identical(as.numeric(gaussian_trend), as.numeric(gaussian_custom)))

  # Test custom sigma via params
  gaussian_sigma <- extract_trends(
    ts_data,
    methods = "gaussian",
    window = 7,
    params = list(gaussian_sigma = 2.0),
    .quiet = TRUE
  )
  expect_s3_class(gaussian_sigma, "ts")

  # Test right alignment using unified align parameter
  gaussian_right <- extract_trends(
    ts_data,
    methods = "gaussian",
    window = 7,
    align = "right",
    .quiet = TRUE
  )
  expect_s3_class(gaussian_right, "ts")

  # Test invalid window (even number)
  expect_error(
    extract_trends(ts_data, methods = "gaussian", window = 6, .quiet = TRUE),
    "Gaussian filter window must be odd"
  )

  # Test invalid align
  expect_error(
    extract_trends(
      ts_data,
      methods = "gaussian",
      params = list(gaussian_align = "invalid"),
      .quiet = TRUE
    ),
    "align must be 'center' or 'right'"
  )
})

test_that("MA alignment options work correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test SMA with different alignments using unified align parameter
  ma_center <- extract_trends(
    ts_data,
    methods = "ma",
    window = 5,
    align = "center",
    .quiet = TRUE
  )
  expect_s3_class(ma_center, "ts")

  ma_left <- extract_trends(
    ts_data,
    methods = "ma",
    window = 5,
    align = "left",
    .quiet = TRUE
  )
  expect_s3_class(ma_left, "ts")

  ma_right <- extract_trends(
    ts_data,
    methods = "ma",
    window = 5,
    align = "right",
    .quiet = TRUE
  )
  expect_s3_class(ma_right, "ts")

  # Different alignments should produce different NA patterns
  expect_false(identical(is.na(ma_center), is.na(ma_left)))
  expect_false(identical(is.na(ma_center), is.na(ma_right)))
  expect_false(identical(is.na(ma_left), is.na(ma_right)))

  # Test WMA with different alignments using unified align parameter
  wma_center <- extract_trends(
    ts_data,
    methods = "wma",
    window = 5,
    align = "center",
    .quiet = TRUE
  )
  expect_s3_class(wma_center, "ts")

  wma_right <- extract_trends(
    ts_data,
    methods = "wma",
    window = 5,
    align = "right",
    .quiet = TRUE
  )
  expect_s3_class(wma_right, "ts")

  # Different alignments should produce different results
  expect_false(identical(as.numeric(wma_center), as.numeric(wma_right)))

  # Test invalid alignment
  expect_error(
    extract_trends(
      ts_data,
      methods = "ma",
      align = "invalid",
      .quiet = TRUE
    ),
    "must be one of"
  )
})

test_that("New filters work with multiple methods", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test multiple new filters together
  new_trends <- extract_trends(
    ts_data,
    methods = c("median", "gaussian"),
    window = 7,
    .quiet = TRUE
  )

  expect_type(new_trends, "list")
  expect_equal(length(new_trends), 2)
  expect_true(all(c("median", "gaussian") %in% names(new_trends)))

  # All should be ts objects with correct length
  for (trend in new_trends) {
    expect_s3_class(trend, "ts")
    expect_equal(length(trend), length(ts_data))
  }

  # Test traditional and new filters together
  mixed_trends <- extract_trends(
    ts_data,
    methods = c("ma", "median", "gaussian"),
    window = 5,
    .quiet = TRUE
  )

  expect_type(mixed_trends, "list")
  expect_equal(length(mixed_trends), 3)
  expect_true(all(c("ma", "median", "gaussian") %in% names(mixed_trends)))
})