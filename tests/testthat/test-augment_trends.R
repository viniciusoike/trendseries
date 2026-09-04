test_that("augment_trends basic functionality works", {
  # Test with quarterly GDP data
  result <- augment_trends(
    gdp_construction,
    value_col = "index",
    methods = "hp"
  )

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
    augment_trends(
      gdp_construction,
      value_col = "index",
      methods = "invalid_method"
    ),
    "Invalid methods"
  )
})

test_that("deprecated group_vars warns and maps to group_cols", {
  test_data <- data.frame(
    date = rep(
      seq.Date(as.Date("2020-01-01"), by = "month", length.out = 36),
      2
    ),
    group = rep(c("A", "B"), each = 36),
    value = rnorm(72, 100, 10)
  )

  expect_warning(
    result <- augment_trends(
      test_data,
      group_vars = "group",
      methods = "ma",
      .quiet = TRUE
    ),
    "deprecated"
  )

  expect_true("trend_ma" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
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
  result <- augment_trends(
    gdp_construction,
    value_col = "index",
    methods = "hp",
    .quiet = TRUE
  )
  expect_s3_class(result, "tbl_df")

  result_monthly <- augment_trends(
    ibcbr,
    value_col = "index",
    methods = "hp",
    .quiet = TRUE
  )
  expect_s3_class(result_monthly, "tbl_df")
})

test_that("augment_trends handles naming conflicts", {
  # First add an HP trend
  result1 <- augment_trends(
    gdp_construction,
    value_col = "index",
    methods = "hp",
    .quiet = TRUE
  )

  # Add another HP trend (should warn and create a new column name)
  expect_warning(
    result2 <- augment_trends(
      result1,
      value_col = "index",
      methods = "hp",
      .quiet = TRUE
    ),
    "already exists"
  )

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
    augment_trends(
      short_data,
      value_col = "index",
      methods = "hp",
      .quiet = TRUE
    ),
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
  constant_data$value <- 100 # All the same value

  # Should still return something, even if trends are NA
  result <- augment_trends(constant_data, methods = "hp", .quiet = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(constant_data))
})

test_that("augment_trends preserves interleaved input row order", {
  panel <- rbind(
    transform(gdp_construction, group = "zebra"),
    transform(gdp_construction, group = "alpha")
  )
  panel <- panel[order(panel$date, panel$group, decreasing = TRUE), ]
  panel$id <- seq_len(nrow(panel))

  result <- augment_trends(
    panel,
    value_col = "index",
    group_cols = "group",
    methods = "hp",
    .quiet = TRUE
  )

  expect_identical(result$id, panel$id)
})
test_that("augment_trends reports a fallback raised by a filter", {
  expect_warning(
    augment_trends(gdp_construction, value_col = "index", methods = "ucm"),
    "UCM estimation failed"
  )
})

test_that("a warning from a grouped call names the groups it came from", {
  panel <- rbind(
    transform(gdp_construction, group = "alpha"),
    transform(gdp_construction, group = "beta")
  )

  expect_warning(
    augment_trends(
      panel,
      value_col = "index",
      group_cols = "group",
      methods = "ucm"
    ),
    "Affected groups"
  )
})

test_that("a repeated warning is reported once for the whole call", {
  panel <- rbind(
    transform(gdp_construction, group = "alpha"),
    transform(gdp_construction, group = "beta")
  )

  warnings <- withCallingHandlers(
    {
      collected <- character()
      suppressMessages(augment_trends(
        panel,
        value_col = "index",
        group_cols = "group",
        frequency = 6,
        methods = "hp"
      ))
      collected
    },
    warning = function(cnd) {
      collected <<- c(collected, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(grep("optimized for standard", warnings), 1)
})

test_that(".quiet suppresses the warnings a filter raises", {
  expect_no_warning(
    augment_trends(
      gdp_construction,
      value_col = "index",
      methods = "ucm",
      .quiet = TRUE
    )
  )
})

test_that("rows with a missing group value keep their own series", {
  panel <- rbind(
    transform(gdp_construction, group = "alpha"),
    transform(gdp_construction, group = NA_character_)
  )

  result <- augment_trends(
    panel,
    value_col = "index",
    group_cols = "group",
    methods = "ma",
    window = 4,
    .quiet = TRUE
  )

  missing_group <- is.na(result$group)
  expect_equal(nrow(result), nrow(panel))
  expect_false(all(is.na(result$trend_ma[missing_group])))
  expect_equal(
    result$trend_ma[missing_group],
    result$trend_ma[!missing_group]
  )
})

test_that("vector window creates separate trend columns for ma", {
  vehicles_recent <- tail(vehicles, 60)
  result <- augment_trends(
    vehicles_recent,
    value_col = "production",
    methods = "ma",
    window = c(3, 6, 12),
    .quiet = TRUE
  )
  expect_true(all(
    c("trend_ma_3", "trend_ma_6", "trend_ma_12") %in% names(result)
  ))
  expect_false("trend_ma" %in% names(result))
})

test_that("vector window with mixed methods: non-MA runs once", {
  vehicles_recent <- tail(vehicles, 60)
  result <- augment_trends(
    vehicles_recent,
    value_col = "production",
    methods = c("hp", "ma"),
    window = c(3, 6),
    .quiet = TRUE
  )
  expect_true(all(c("trend_hp", "trend_ma_3", "trend_ma_6") %in% names(result)))
  expect_false("trend_hp_3" %in% names(result))
  expect_false("trend_ma" %in% names(result))
})

test_that("several values, methods, and windows are assembled together", {
  data <- tail(vehicles, 60)
  data$production_2 <- data$production * 2

  result <- augment_trends(
    data,
    value_col = c("production", "production_2"),
    methods = c("hp", "ma"),
    window = c(3, 6),
    suffix = "baseline",
    .quiet = TRUE
  )

  expect_equal(
    setdiff(names(result), names(data)),
    c(
      "trend_hp_production_baseline",
      "trend_ma_3_production_baseline",
      "trend_ma_6_production_baseline",
      "trend_hp_production_2_baseline",
      "trend_ma_3_production_2_baseline",
      "trend_ma_6_production_2_baseline"
    )
  )
  expect_equal(
    result$trend_ma_3_production_2_baseline,
    result$trend_ma_3_production_baseline * 2
  )
})

test_that("extract_trends is called once per group and value column", {
  data <- tail(vehicles, 60)
  data$value_2 <- data$production * 2
  panel <- rbind(
    transform(data, group = "a"),
    transform(data, group = "b")
  )
  original_extract_trends <- extract_trends
  calls <- 0L
  local_mocked_bindings(
    extract_trends = function(...) {
      calls <<- calls + 1L
      original_extract_trends(...)
    }
  )

  augment_trends(
    panel,
    value_col = c("production", "value_2"),
    group_cols = "group",
    methods = c("hp", "ma"),
    window = c(3, 6),
    .quiet = TRUE
  )

  expect_identical(calls, 4L)
})

test_that("vector window with suffix combines correctly", {
  vehicles_recent <- tail(vehicles, 60)
  result <- augment_trends(
    vehicles_recent,
    value_col = "production",
    methods = "ma",
    window = c(3, 6),
    suffix = "v1",
    .quiet = TRUE
  )
  expect_true(all(c("trend_ma_3_v1", "trend_ma_6_v1") %in% names(result)))
})

test_that("vector window for non-MA method warns and uses first value", {
  vehicles_recent <- tail(vehicles, 60)
  expect_warning(
    augment_trends(
      vehicles_recent,
      value_col = "production",
      methods = "hp",
      window = c(3, 6)
    ),
    "only supported for"
  )
})

test_that("vector window works with median method", {
  vehicles_recent <- tail(vehicles, 60)
  result <- augment_trends(
    vehicles_recent,
    value_col = "production",
    methods = "median",
    window = c(3, 7),
    .quiet = TRUE
  )
  expect_true(all(c("trend_median_3", "trend_median_7") %in% names(result)))
})

## Irregular daily series -----------------------------------------------------

test_that("trends on an irregular daily series land on the input dates", {
  coffee <- coffee_arabica[, c("date", "usd_2022")]

  result <- augment_trends(
    coffee,
    value_col = "usd_2022",
    methods = c("stl", "ma"),
    window = 22,
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(coffee))
  expect_equal(result$date, coffee$date)
  expect_false(all(is.na(result$trend_stl)))
  expect_false(all(is.na(result$trend_ma)))

  # Only the two half-windows the centred average cannot cover stay missing
  expect_equal(sum(is.na(result$trend_ma)), 22)
})

test_that("a daily moving average matches the same window computed by hand", {
  coffee <- coffee_arabica[, c("date", "usd_2022")]

  result <- augment_trends(
    coffee,
    value_col = "usd_2022",
    methods = "ma",
    window = 22,
    .quiet = TRUE
  )

  # 2x22 centred average: 21 interior values plus half of each endpoint
  values <- coffee$usd_2022
  expected <- (sum(values[1990:2010]) + (values[1989] + values[2011]) / 2) / 22

  expect_equal(result$trend_ma[2000], expected)
})

test_that("grouped irregular daily series keep one row per input row", {
  coffee <- rbind(
    data.frame(crop = "arabica", coffee_arabica[, c("date", "usd_2022")]),
    data.frame(crop = "robusta", coffee_robusta[, c("date", "usd_2022")])
  )

  result <- augment_trends(
    coffee,
    value_col = "usd_2022",
    group_cols = "crop",
    methods = "ma",
    window = 22,
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(coffee))
  expect_equal(sort(table(result$crop)), sort(table(coffee$crop)))
  expect_false(any(tapply(result$trend_ma, result$crop, function(x) {
    all(is.na(x))
  })))
})

test_that("a semi-annual series is not multiplied by the merge", {
  semi <- data.frame(
    date = seq(as.Date("2000-01-01"), by = "6 months", length.out = 20),
    value = as.numeric(1:20)
  )

  result <- augment_trends(
    semi,
    frequency = 2,
    methods = "ma",
    window = 3,
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(semi))
  expect_equal(result$date, semi$date)
  expect_false(all(is.na(result$trend_ma)))
})

test_that("a repeated date in a daily series is rejected", {
  dup <- data.frame(
    date = as.Date("2020-01-01") + c(0, 1, 1, 4, 5, 6, 7, 8),
    value = as.numeric(1:8)
  )

  expect_error(
    augment_trends(
      dup,
      frequency = 252,
      methods = "ma",
      window = 3,
      .quiet = TRUE
    ),
    "duplicated date"
  )
})

test_that("the bundled coffee datasets carry the moving average they document", {
  for (coffee in list(coffee_arabica, coffee_robusta)) {
    expect_type(coffee$trend_ma, "double")

    # The first 21 observations have no full window; the rest are populated
    expect_equal(which(!is.na(coffee$trend_ma))[1], 22L)
    expect_false(anyNA(coffee$trend_ma[22:nrow(coffee)]))

    recomputed <- augment_trends(
      coffee[, c("date", "usd_2022")],
      value_col = "usd_2022",
      methods = "ma",
      window = 22,
      align = "right",
      .quiet = TRUE
    )
    expect_equal(recomputed$trend_ma, coffee$trend_ma)
  }
})
