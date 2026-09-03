# Tests for detrend_series()
# Data used:
#   gdp_construction  — quarterly, value_col = "index"

tol <- 1e-10

# Input validation ----------------------------------------------------------

test_that("detrend_series errors on invalid method", {
  expect_error(
    detrend_series(gdp_construction, value_col = "index", methods = "nope"),
    "Invalid method"
  )
})

test_that("detrend_series errors on invalid transform", {
  expect_error(
    detrend_series(gdp_construction, value_col = "index", transform = "boxcox"),
    "Invalid transform"
  )
})

test_that("detrend_series errors on non-logical components", {
  expect_error(
    detrend_series(gdp_construction, value_col = "index", components = "yes"),
    "components"
  )
})

test_that("detrend_series errors on bad data and missing columns", {
  expect_error(detrend_series(list(a = 1)), "data.frame")
  expect_error(
    detrend_series(gdp_construction, value_col = "nonexistent"),
    "not found"
  )
})

test_that("detrend_series log transform requires positive values", {
  df <- gdp_construction
  df$index[5] <- -1
  expect_error(
    detrend_series(df, value_col = "index", transform = "log"),
    "strictly positive"
  )
})

# Default behaviour: only the detrended column is added ----------------------

test_that("detrend_series adds only detrend_hp column by default", {
  result <- detrend_series(gdp_construction, value_col = "index", .quiet = TRUE)
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(new_cols, "detrend_hp")
  expect_false("trend_hp" %in% names(result))
  expect_true(is.numeric(result$detrend_hp))
})

test_that("detrend_series matches value minus augment_trends trend", {
  full <- augment_trends(
    gdp_construction,
    value_col = "index",
    methods = "hp",
    .quiet = TRUE
  )
  result <- detrend_series(gdp_construction, value_col = "index", .quiet = TRUE)
  expect_equal(result$detrend_hp, full$index - full$trend_hp, tolerance = tol)
})

# components = TRUE keeps the trend ------------------------------------------

test_that("detrend_series with components = TRUE keeps the trend column", {
  result <- detrend_series(
    gdp_construction,
    value_col = "index",
    components = TRUE,
    .quiet = TRUE
  )
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(sort(new_cols), sort(c("trend_hp", "detrend_hp")))
  # Additive identity holds
  expect_equal(
    result$trend_hp + result$detrend_hp,
    result$index,
    tolerance = tol
  )
})

# Multiple methods ------------------------------------------------------------

test_that("detrend_series supports multiple methods", {
  result <- detrend_series(
    gdp_construction,
    value_col = "index",
    methods = c("hp", "stl"),
    .quiet = TRUE
  )
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(sort(new_cols), sort(c("detrend_hp", "detrend_stl")))
})

# Log transform: multiplicative identity, original values untouched -----------

test_that("detrend_series log transform yields multiplicative identity", {
  result <- detrend_series(
    gdp_construction,
    value_col = "index",
    transform = "log",
    components = TRUE,
    .quiet = TRUE
  )
  expect_equal(
    result$trend_hp * exp(result$detrend_hp),
    result$index,
    tolerance = tol
  )
})

test_that("detrend_series log transform leaves the value column unchanged", {
  result <- detrend_series(
    gdp_construction,
    value_col = "index",
    transform = "log",
    .quiet = TRUE
  )
  expect_equal(
    result$index[order(result$date)],
    gdp_construction$index[order(gdp_construction$date)]
  )
  expect_false(any(startsWith(names(result), ".detrend_log_fit")))
})

test_that("detrend_series log matches log-scale subtraction", {
  df <- gdp_construction
  df$log_index <- log(df$index)
  by_hand <- detrend_series(df, value_col = "log_index", .quiet = TRUE)
  result <- detrend_series(
    df,
    value_col = "index",
    transform = "log",
    .quiet = TRUE
  )
  expect_equal(result$detrend_hp, by_hand$detrend_hp, tolerance = tol)
})

# Unified parameters pass through ---------------------------------------------

test_that("detrend_series passes window through to augment_trends", {
  result <- detrend_series(
    gdp_construction,
    value_col = "index",
    methods = "ma",
    window = c(4, 8),
    .quiet = TRUE
  )
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(sort(new_cols), sort(c("detrend_ma_4", "detrend_ma_8")))
})

# Multiple value columns -------------------------------------------------------

test_that("detrend_series detrends multiple value columns", {
  df <- gdp_construction
  df$index2 <- df$index * 2
  result <- detrend_series(
    df,
    value_col = c("index", "index2"),
    .quiet = TRUE
  )
  new_cols <- setdiff(names(result), names(df))
  expect_equal(sort(new_cols), sort(c("detrend_hp_index", "detrend_hp_index2")))
})

# Pre-existing columns are never dropped or overwritten ------------------------

test_that("detrend_series preserves a pre-existing trend column", {
  df <- gdp_construction
  df$trend_hp <- 0
  expect_warning(
    result <- detrend_series(df, value_col = "index", .quiet = TRUE),
    "already exists"
  )
  # The user's column survives untouched; the renamed fitted trend is dropped
  expect_true("trend_hp" %in% names(result))
  expect_equal(unique(result$trend_hp), 0)
  expect_false("trend_hp_1" %in% names(result))
  # The detrended column mirrors the renamed trend column it derives from
  expect_true("detrend_hp_1" %in% names(result))
})

test_that("detrend_series renames on detrend column conflict", {
  df <- gdp_construction
  df$detrend_hp <- 0
  expect_warning(
    result <- detrend_series(df, value_col = "index", .quiet = TRUE),
    "already exists"
  )
  expect_equal(unique(result$detrend_hp), 0)
  expect_true("detrend_hp_1" %in% names(result))
})

# Grouped detrending -----------------------------------------------------------

test_that("detrend_series works with group_cols", {
  grp_a <- gdp_construction
  grp_a$sector <- "A"
  grp_b <- gdp_construction
  grp_b$sector <- "B"
  panel <- rbind(grp_a, grp_b)

  result <- detrend_series(
    panel,
    value_col = "index",
    group_cols = "sector",
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(panel))
  new_cols <- setdiff(names(result), names(panel))
  expect_equal(new_cols, "detrend_hp")
  expect_false(anyNA(result$detrend_hp))
})

# .quiet suppresses messages ----------------------------------------------------

test_that("detrend_series with .quiet = TRUE emits no messages", {
  expect_silent(
    detrend_series(gdp_construction, value_col = "index", .quiet = TRUE)
  )
})
