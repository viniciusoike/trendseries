test_that("augment_trends basic functionality works", {
  # Test with quarterly GDP data
  result <- augment_trends(gdp_brazil_qtr, methods = "hp")

  expect_s3_class(result, "tbl_df")
  expect_true("trend_hp" %in% names(result))
  expect_equal(nrow(result), nrow(gdp_brazil_qtr))
  expect_true(all(names(gdp_brazil_qtr) %in% names(result)))
})

test_that("augment_trends handles multiple methods", {
  result <- augment_trends(
    gdp_brazil_qtr,
    methods = c("hp", "ma", "poly"),
    .quiet = TRUE
  )

  expect_true(all(c("trend_hp", "trend_ma", "trend_poly") %in% names(result)))
})

test_that("augment_trends validates inputs correctly", {
  # Invalid data type
  expect_error(
    augment_trends(list(a = 1, b = 2)),
    "data.frame"
  )

  # Missing date column
  expect_error(
    augment_trends(gdp_brazil_qtr, date_col = "nonexistent"),
    "not found"
  )

  # Missing value column
  expect_error(
    augment_trends(gdp_brazil_qtr, value_col = "nonexistent"),
    "not found"
  )

  # Invalid method
  expect_error(
    augment_trends(gdp_brazil_qtr, methods = "invalid_method"),
    "Invalid methods"
  )
})

test_that("augment_trends handles custom column names", {
  # Create test data with different column names
  test_data <- gdp_brazil_qtr
  names(test_data)[1:2] <- c("time", "gdp")

  result <- augment_trends(
    test_data,
    date_col = "time",
    value_col = "gdp",
    methods = "hp",
    .quiet = TRUE
  )

  expect_true("trend_hp" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
})

test_that("augment_trends handles frequency detection", {
  result <- augment_trends(gdp_brazil_qtr, methods = "hp", .quiet = TRUE)
  expect_s3_class(result, "tbl_df")

  result_monthly <- augment_trends(ibcbr, methods = "hp", .quiet = TRUE)
  expect_s3_class(result_monthly, "tbl_df")
})

test_that("augment_trends handles naming conflicts", {
  # First add an HP trend
  result1 <- augment_trends(gdp_brazil_qtr, methods = "hp", .quiet = TRUE)

  # Add another HP trend (should create new column name)
  result2 <- augment_trends(result1, methods = "hp", .quiet = TRUE)

  # Should have both trend_hp and trend_hp_1 (or similar)
  trend_cols <- grep("^trend_hp", names(result2), value = TRUE)
  expect_true(length(trend_cols) >= 2)
})

test_that("augment_trends custom parameters work", {
  result <- augment_trends(
    gdp_brazil_qtr,
    methods = c("hp", "ma"),
    hp_lambda = 1000,
    ma_window = 8,
    .quiet = TRUE
  )

  expect_true(all(c("trend_hp", "trend_ma") %in% names(result)))
})

test_that("augment_trends handles short series", {
  # Create very short series
  short_data <- gdp_brazil_qtr[1:5, ]

  expect_warning(
    augment_trends(short_data, methods = "hp", .quiet = TRUE),
    "observations"
  )
})

test_that("augment_trends suffix parameter works", {
  result <- augment_trends(
    gdp_brazil_qtr,
    methods = c("hp", "ma"),
    suffix = "test",
    .quiet = TRUE
  )

  expect_true(all(c("trend_hp_test", "trend_ma_test") %in% names(result)))
})

test_that("augment_trends returns original data when trends fail", {
  # This test ensures robustness when trend extraction fails
  # We'll use a constant series which might cause issues for some methods
  constant_data <- gdp_brazil_qtr
  constant_data$value <- 100  # All the same value

  # Should still return something, even if trends are NA
  result <- augment_trends(constant_data, methods = "hp", .quiet = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(constant_data))
})