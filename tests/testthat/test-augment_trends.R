test_that("augment_trends basic functionality works", {
  # Test with quarterly GDP data
  result <- augment_trends(gdp_construction, value_col = "index", methods = "hp")

  expect_s3_class(result, "tbl_df")
  expect_true("trend_hp" %in% names(result))
  expect_equal(nrow(result), nrow(gdp_construction))
  expect_true(all(names(gdp_construction) %in% names(result)))
})

test_that("augment_trends handles multiple methods", {
  result <- augment_trends(
    gdp_construction,
    value_col = "index",
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
    augment_trends(gdp_construction, date_col = "nonexistent"),
    "not found"
  )

  # Missing value column
  expect_error(
    augment_trends(gdp_construction, value_col = "nonexistent"),
    "not found"
  )

  # Invalid method
  expect_error(
    augment_trends(gdp_construction, value_col = "index", methods = "invalid_method"),
    "Invalid methods"
  )
})

test_that("augment_trends handles custom column names", {
  # Create test data with different column names
  test_data <- gdp_construction
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
  result <- augment_trends(gdp_construction, value_col = "index", methods = "hp", .quiet = TRUE)
  expect_s3_class(result, "tbl_df")

  result_monthly <- augment_trends(ibcbr, value_col = "index", methods = "hp", .quiet = TRUE)
  expect_s3_class(result_monthly, "tbl_df")
})

test_that("augment_trends handles naming conflicts", {
  # First add an HP trend
  result1 <- augment_trends(gdp_construction, value_col = "index", methods = "hp", .quiet = TRUE)

  # Add another HP trend (should create new column name)
  result2 <- augment_trends(result1, value_col = "index", methods = "hp", .quiet = TRUE)

  # Should have both trend_hp and trend_hp_1 (or similar)
  trend_cols <- grep("^trend_hp", names(result2), value = TRUE)
  expect_true(length(trend_cols) >= 2)
})

test_that("augment_trends custom parameters work", {
  result <- augment_trends(
    gdp_construction,
    value_col = "index",
    methods = c("hp", "ma"),
    smoothing = 1000,
    window = 8,
    .quiet = TRUE
  )

  expect_true(all(c("trend_hp", "trend_ma") %in% names(result)))
})

test_that("augment_trends handles short series", {
  # Create very short series
  short_data <- gdp_construction[1:5, ]

  expect_warning(
    augment_trends(short_data, value_col = "index", methods = "hp", .quiet = TRUE),
    "observations"
  )
})

test_that("augment_trends suffix parameter works", {
  result <- augment_trends(
    gdp_construction,
    value_col = "index",
    methods = c("hp", "ma"),
    suffix = "test",
    .quiet = TRUE
  )

  expect_true(all(c("trend_hp_test", "trend_ma_test") %in% names(result)))
})

test_that("augment_trends returns original data when trends fail", {
  # This test ensures robustness when trend extraction fails
  # We'll use a constant series which might cause issues for some methods
  constant_data <- gdp_construction
  constant_data$value <- 100  # All the same value

  # Should still return something, even if trends are NA
  result <- augment_trends(constant_data, methods = "hp", .quiet = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(constant_data))
})
test_that("vector window creates separate trend columns for ma", {
  vehicles_recent <- tail(vehicles, 60)
  result <- augment_trends(
    vehicles_recent, value_col = "production",
    methods = "ma", window = c(3, 6, 12), .quiet = TRUE
  )
  expect_true(all(c("trend_ma_3", "trend_ma_6", "trend_ma_12") %in% names(result)))
  expect_false("trend_ma" %in% names(result))
})

test_that("vector window with mixed methods: non-MA runs once", {
  vehicles_recent <- tail(vehicles, 60)
  result <- augment_trends(
    vehicles_recent, value_col = "production",
    methods = c("hp", "ma"), window = c(3, 6), .quiet = TRUE
  )
  expect_true(all(c("trend_hp", "trend_ma_3", "trend_ma_6") %in% names(result)))
  expect_false("trend_hp_3" %in% names(result))
  expect_false("trend_ma" %in% names(result))
})

test_that("vector window with suffix combines correctly", {
  vehicles_recent <- tail(vehicles, 60)
  result <- augment_trends(
    vehicles_recent, value_col = "production",
    methods = "ma", window = c(3, 6), suffix = "v1", .quiet = TRUE
  )
  expect_true(all(c("trend_ma_3_v1", "trend_ma_6_v1") %in% names(result)))
})

test_that("vector window for non-MA method warns and uses first value", {
  vehicles_recent <- tail(vehicles, 60)
  expect_warning(
    augment_trends(
      vehicles_recent, value_col = "production",
      methods = "hp", window = c(3, 6)
    ),
    "only supported for"
  )
})

test_that("vector window works with median method", {
  vehicles_recent <- tail(vehicles, 60)
  result <- augment_trends(
    vehicles_recent, value_col = "production",
    methods = "median", window = c(3, 7), .quiet = TRUE
  )
  expect_true(all(c("trend_median_3", "trend_median_7") %in% names(result)))
})
