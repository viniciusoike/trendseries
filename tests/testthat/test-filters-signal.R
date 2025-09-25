test_that("Kalman filter works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality with automatic noise estimation
  kalman_trend <- extract_trends(ts_data, methods = "kalman", .quiet = TRUE)
  expect_s3_class(kalman_trend, "ts")
  expect_equal(length(kalman_trend), length(ts_data))
  expect_false(any(is.na(kalman_trend)))

  # Test with custom noise parameters
  kalman_custom <- extract_trends(
    ts_data,
    methods = "kalman",
    params = list(
      kalman_measurement_noise = 1000,
      kalman_process_noise = 100
    ),
    .quiet = TRUE
  )
  expect_s3_class(kalman_custom, "ts")
  expect_false(identical(as.numeric(kalman_trend), as.numeric(kalman_custom)))
})

test_that("Savitzky-Golay filter works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  sg_trend <- extract_trends(ts_data, methods = "sg", .quiet = TRUE)
  expect_s3_class(sg_trend, "ts")
  expect_equal(length(sg_trend), length(ts_data))

  # Test custom window and polynomial order
  sg_custom <- extract_trends(
    ts_data,
    methods = "sg",
    window = 11,
    params = list(sg_poly_order = 3),
    .quiet = TRUE
  )
  expect_s3_class(sg_custom, "ts")
  expect_false(identical(as.numeric(sg_trend), as.numeric(sg_custom)))
})

test_that("Kernel smoothing works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality with automatic bandwidth
  kernel_trend <- extract_trends(ts_data, methods = "kernel", .quiet = TRUE)
  expect_s3_class(kernel_trend, "ts")
  expect_equal(length(kernel_trend), length(ts_data))
  expect_false(any(is.na(kernel_trend)))

  # Test custom bandwidth
  kernel_custom <- extract_trends(
    ts_data,
    methods = "kernel",
    smoothing = 0.2,
    .quiet = TRUE
  )
  expect_s3_class(kernel_custom, "ts")
  expect_false(identical(as.numeric(kernel_trend), as.numeric(kernel_custom)))
})

test_that("Butterworth filter works correctly", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test basic functionality
  butter_trend <- extract_trends(ts_data, methods = "butter", .quiet = TRUE)
  expect_s3_class(butter_trend, "ts")
  expect_equal(length(butter_trend), length(ts_data))

  # Test custom cutoff and order
  butter_custom <- extract_trends(
    ts_data,
    methods = "butter",
    band = c(0.05, 3),
    .quiet = TRUE
  )
  expect_s3_class(butter_custom, "ts")
  expect_false(identical(as.numeric(butter_trend), as.numeric(butter_custom)))
})

test_that("Signal processing methods handle edge cases", {
  # Test with short series
  short_ts <- ts(1:20, frequency = 12)

  # Kalman should work with short series
  kalman_short <- extract_trends(short_ts, methods = "kalman", .quiet = TRUE)
  expect_s3_class(kalman_short, "ts")

  # SG should fail with window larger than series length
  expect_error(
    extract_trends(short_ts, methods = "sg", window = 25, .quiet = TRUE)
  )
})

test_that("Multiple signal processing methods work together", {
  ts_data <- df_to_ts(vehicles, value_col = "vehicles", frequency = 12)

  # Test multiple methods
  signal_trends <- extract_trends(
    ts_data,
    methods = c("kalman", "sg", "butter"),
    .quiet = TRUE
  )

  expect_type(signal_trends, "list")
  expect_equal(length(signal_trends), 3)
  expect_true(all(c("kalman", "sg", "butter") %in% names(signal_trends)))

  # All should be ts objects
  for (trend in signal_trends) {
    expect_s3_class(trend, "ts")
    expect_equal(length(trend), length(ts_data))
  }
})