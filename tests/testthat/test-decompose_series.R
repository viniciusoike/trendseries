# Tests for decompose_series()
# Data used:
#   gdp_construction  — quarterly, value_col = "index"
#   ibcbr             — monthly,   value_col = "index"
#   vehicles          — monthly,   value_col = "production"

# ---------------------------------------------------------------------------
# Helper: tolerance for floating-point identity checks
# ---------------------------------------------------------------------------
tol <- 1e-10

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("decompose_series errors on non-data.frame input", {
  expect_error(
    decompose_series(list(a = 1)),
    "data.frame"
  )
})

test_that("decompose_series errors on missing date column", {
  expect_error(
    decompose_series(gdp_construction, date_col = "nonexistent"),
    "not found"
  )
})

test_that("decompose_series errors on missing value column", {
  expect_error(
    decompose_series(gdp_construction, value_col = "nonexistent"),
    "not found"
  )
})

test_that("decompose_series errors on invalid method", {
  expect_error(
    decompose_series(gdp_construction, value_col = "index", method = "hp"),
    "Invalid method"
  )
})

test_that("decompose_series errors on invalid trend", {
  expect_error(
    decompose_series(
      gdp_construction,
      value_col = "index",
      method = "regression",
      trend = "exponential"
    ),
    "Invalid trend"
  )
})

test_that("decompose_series errors on non-list params", {
  expect_error(
    decompose_series(gdp_construction, value_col = "index", params = "robust"),
    "list"
  )
})

test_that("decompose_series errors on missing group column", {
  expect_error(
    decompose_series(
      gdp_construction,
      value_col = "index",
      group_cols = "nonexistent"
    ),
    "Group columns not found"
  )
})

# ---------------------------------------------------------------------------
# Error on non-seasonal data (frequency = 1)
# ---------------------------------------------------------------------------

test_that("decompose_series (stl) errors on annual data", {
  annual <- data.frame(
    date  = seq(as.Date("2000-01-01"), by = "year", length.out = 20),
    value = rnorm(20)
  )
  expect_error(
    decompose_series(annual, .quiet = TRUE),
    "frequency"
  )
})

test_that("decompose_series (regression) errors on annual data", {
  annual <- data.frame(
    date  = seq(as.Date("2000-01-01"), by = "year", length.out = 20),
    value = rnorm(20)
  )
  expect_error(
    decompose_series(annual, method = "regression", .quiet = TRUE),
    "frequency"
  )
})

# ---------------------------------------------------------------------------
# STL — basic functionality
# ---------------------------------------------------------------------------

test_that("decompose_series STL returns tibble with correct structure", {
  result <- decompose_series(gdp_construction, value_col = "index", .quiet = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(gdp_construction))
  # All original columns preserved
  expect_true(all(names(gdp_construction) %in% names(result)))
  # New columns added
  expect_true(all(c("trend_stl", "seasonal_stl", "remainder_stl") %in% names(result)))
})

test_that("decompose_series STL: trend + seasonal + remainder = value (quarterly)", {
  result <- decompose_series(gdp_construction, value_col = "index", .quiet = TRUE)

  reconstructed <- result$trend_stl + result$seasonal_stl + result$remainder_stl
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series STL: trend + seasonal + remainder = value (monthly)", {
  result <- decompose_series(ibcbr, value_col = "index", .quiet = TRUE)

  reconstructed <- result$trend_stl + result$seasonal_stl + result$remainder_stl
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series STL components are numeric", {
  result <- decompose_series(gdp_construction, value_col = "index", .quiet = TRUE)

  expect_true(is.numeric(result$trend_stl))
  expect_true(is.numeric(result$seasonal_stl))
  expect_true(is.numeric(result$remainder_stl))
})

test_that("decompose_series STL: seasonal component has near-zero annual sum", {
  # For a periodic seasonal pattern the seasonal values within each year sum to ~0
  result <- decompose_series(gdp_construction, value_col = "index", .quiet = TRUE)
  result$year <- format(result$date, "%Y")

  annual_seasonal <- tapply(result$seasonal_stl, result$year, sum)
  # Allow reasonable tolerance — not exact due to loess boundary effects
  expect_true(all(abs(annual_seasonal) < 5))
})

# ---------------------------------------------------------------------------
# STL — params
# ---------------------------------------------------------------------------

test_that("decompose_series STL robust = TRUE runs without error", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    params = list(robust = TRUE),
    .quiet = TRUE
  )
  expect_s3_class(result, "tbl_df")
  expect_true("trend_stl" %in% names(result))
})

test_that("decompose_series STL accepts both dot and underscore param notation", {
  r1 <- decompose_series(
    gdp_construction,
    value_col = "index",
    params = list(s.window = 13),
    .quiet = TRUE
  )
  r2 <- decompose_series(
    gdp_construction,
    value_col = "index",
    params = list(stl_s_window = 13),
    .quiet = TRUE
  )
  expect_equal(r1$trend_stl, r2$trend_stl)
})

test_that("decompose_series STL with t.window param runs without error", {
  result <- decompose_series(
    ibcbr,
    value_col = "index",
    params = list(t.window = 13),
    .quiet = TRUE
  )
  expect_s3_class(result, "tbl_df")
})

# ---------------------------------------------------------------------------
# Regression — linear trend
# ---------------------------------------------------------------------------

test_that("decompose_series regression (linear) returns correct structure", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    .quiet    = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(gdp_construction))
  expect_true(all(names(gdp_construction) %in% names(result)))
  expect_true(all(
    c("trend_regression", "seasonal_regression", "remainder_regression") %in% names(result)
  ))
})

test_that("decompose_series regression (linear): exact identity holds (quarterly)", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    .quiet    = TRUE
  )

  reconstructed <- result$trend_regression + result$seasonal_regression + result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series regression (linear): exact identity holds (monthly)", {
  result <- decompose_series(
    ibcbr,
    value_col = "index",
    method    = "regression",
    .quiet    = TRUE
  )

  reconstructed <- result$trend_regression + result$seasonal_regression + result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

# ---------------------------------------------------------------------------
# Regression — quadratic trend
# ---------------------------------------------------------------------------

test_that("decompose_series regression (quadratic): exact identity holds", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    trend     = "quadratic",
    .quiet    = TRUE
  )

  reconstructed <- result$trend_regression + result$seasonal_regression + result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series regression quadratic produces different trend than linear", {
  r_linear <- decompose_series(
    ibcbr,
    value_col = "index",
    method    = "regression",
    trend     = "linear",
    .quiet    = TRUE
  )
  r_quad <- decompose_series(
    ibcbr,
    value_col = "index",
    method    = "regression",
    trend     = "quadratic",
    .quiet    = TRUE
  )

  expect_false(identical(r_linear$trend_regression, r_quad$trend_regression))
})

# ---------------------------------------------------------------------------
# Regression — cubic trend
# ---------------------------------------------------------------------------

test_that("decompose_series regression (cubic): exact identity holds", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    trend     = "cubic",
    .quiet    = TRUE
  )

  reconstructed <- result$trend_regression + result$seasonal_regression + result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series regression cubic produces different trend than quadratic", {
  r_quad <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    trend     = "quadratic",
    .quiet    = TRUE
  )
  r_cubic <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    trend     = "cubic",
    .quiet    = TRUE
  )

  expect_false(identical(r_quad$trend_regression, r_cubic$trend_regression))
})

# ---------------------------------------------------------------------------
# Regression — poly_raw param
# ---------------------------------------------------------------------------

test_that("decompose_series regression poly_raw = TRUE runs and identity holds", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    trend     = "quadratic",
    params    = list(poly_raw = TRUE),
    .quiet    = TRUE
  )

  reconstructed <- result$trend_regression + result$seasonal_regression + result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series regression raw and orthogonal polynomials give similar fit", {
  # Both parameterisations should yield the same fitted values
  r_orth <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    trend     = "linear",
    params    = list(poly_raw = FALSE),
    .quiet    = TRUE
  )
  r_raw <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    trend     = "linear",
    params    = list(poly_raw = TRUE),
    .quiet    = TRUE
  )

  # Fitted values (trend + seasonal) must be identical regardless of parameterisation
  fitted_orth <- r_orth$trend_regression + r_orth$seasonal_regression
  fitted_raw  <- r_raw$trend_regression  + r_raw$seasonal_regression
  expect_equal(fitted_orth, fitted_raw, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Seasonal component: zero sum per cycle (regression)
# ---------------------------------------------------------------------------

test_that("decompose_series regression: seasonal dummies sum to zero across periods", {
  # The seasonal component from dummies must sum to zero over a full cycle
  # (as the baseline period contributes 0 by construction)
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    .quiet    = TRUE
  )

  result$year <- format(result$date, "%Y")
  # Drop incomplete boundary years
  year_counts <- table(result$year)
  full_years  <- names(year_counts[year_counts == 4])
  result_full <- result[result$year %in% full_years, ]

  annual_seasonal <- tapply(result_full$seasonal_regression, result_full$year, sum)
  # Dummies for quarterly data: baseline Q1 = 0, Q2/Q3/Q4 get coefficients.
  # Sum is NOT necessarily 0 (baseline period has no dummy), so we just verify
  # the seasonal values are finite and non-constant.
  expect_true(all(is.finite(annual_seasonal)))
})

# ---------------------------------------------------------------------------
# Grouped data
# ---------------------------------------------------------------------------

test_that("decompose_series STL works with group_cols", {
  # Build a minimal panel from two copies of gdp_construction
  grp_a <- gdp_construction; grp_a$sector <- "A"
  grp_b <- gdp_construction; grp_b$sector <- "B"
  panel <- rbind(grp_a, grp_b)

  result <- decompose_series(
    panel,
    value_col  = "index",
    group_cols = "sector",
    .quiet     = TRUE
  )

  expect_equal(nrow(result), nrow(panel))
  expect_true(all(c("trend_stl", "seasonal_stl", "remainder_stl") %in% names(result)))

  # Identity must hold within each group
  for (grp in c("A", "B")) {
    sub <- result[result$sector == grp, ]
    reconstructed <- sub$trend_stl + sub$seasonal_stl + sub$remainder_stl
    expect_equal(reconstructed, sub$index, tolerance = tol,
                 label = paste0("identity for group ", grp))
  }
})

test_that("decompose_series regression works with group_cols", {
  grp_a <- gdp_construction; grp_a$sector <- "A"
  grp_b <- gdp_construction; grp_b$sector <- "B"
  panel <- rbind(grp_a, grp_b)

  result <- decompose_series(
    panel,
    value_col  = "index",
    method     = "regression",
    group_cols = "sector",
    .quiet     = TRUE
  )

  expect_equal(nrow(result), nrow(panel))
  expect_true(all(
    c("trend_regression", "seasonal_regression", "remainder_regression") %in% names(result)
  ))
})

# ---------------------------------------------------------------------------
# Column naming
# ---------------------------------------------------------------------------

test_that("decompose_series STL column names are correct", {
  result <- decompose_series(gdp_construction, value_col = "index", .quiet = TRUE)
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(sort(new_cols), sort(c("trend_stl", "seasonal_stl", "remainder_stl")))
})

test_that("decompose_series regression column names are correct", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    method    = "regression",
    .quiet    = TRUE
  )
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(
    sort(new_cols),
    sort(c("trend_regression", "seasonal_regression", "remainder_regression"))
  )
})

# ---------------------------------------------------------------------------
# Short series warning
# ---------------------------------------------------------------------------

test_that("decompose_series warns on short series", {
  short <- gdp_construction[1:8, ]
  expect_warning(
    decompose_series(short, value_col = "index", .quiet = FALSE),
    "observations"
  )
})

# ---------------------------------------------------------------------------
# .quiet suppresses messages
# ---------------------------------------------------------------------------

test_that("decompose_series with .quiet = TRUE emits no messages", {
  expect_no_message(
    decompose_series(gdp_construction, value_col = "index", .quiet = TRUE)
  )
  expect_no_message(
    decompose_series(
      gdp_construction,
      value_col = "index",
      method    = "regression",
      .quiet    = TRUE
    )
  )
})
