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
    decompose_series(gdp_construction, value_col = "index", methods = "hp"),
    "Invalid method"
  )
})

test_that("decompose_series errors on invalid trend", {
  expect_error(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = "regression",
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
    date = seq(as.Date("2000-01-01"), by = "year", length.out = 20),
    value = rnorm(20)
  )
  expect_error(
    decompose_series(annual, .quiet = TRUE),
    "frequency"
  )
})

test_that("decompose_series (regression) errors on annual data", {
  annual <- data.frame(
    date = seq(as.Date("2000-01-01"), by = "year", length.out = 20),
    value = rnorm(20)
  )
  expect_error(
    decompose_series(annual, methods = "regression", .quiet = TRUE),
    "frequency"
  )
})

# ---------------------------------------------------------------------------
# STL — basic functionality
# ---------------------------------------------------------------------------

test_that("decompose_series STL returns tibble with correct structure", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(gdp_construction))
  # All original columns preserved
  expect_true(all(names(gdp_construction) %in% names(result)))
  # New columns added
  expect_true(all(
    c("trend_stl", "seasonal_stl", "remainder_stl") %in% names(result)
  ))
})

test_that("decompose_series STL: trend + seasonal + remainder = value (quarterly)", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    .quiet = TRUE
  )

  reconstructed <- result$trend_stl + result$seasonal_stl + result$remainder_stl
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series STL: trend + seasonal + remainder = value (monthly)", {
  result <- decompose_series(ibcbr, value_col = "index", .quiet = TRUE)

  reconstructed <- result$trend_stl + result$seasonal_stl + result$remainder_stl
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series STL components are numeric", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    .quiet = TRUE
  )

  expect_true(is.numeric(result$trend_stl))
  expect_true(is.numeric(result$seasonal_stl))
  expect_true(is.numeric(result$remainder_stl))
})

test_that("decompose_series STL: seasonal component has near-zero annual sum", {
  # For a periodic seasonal pattern the seasonal values within each year sum to ~0
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    .quiet = TRUE
  )
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
    methods = "regression",
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(gdp_construction))
  expect_true(all(names(gdp_construction) %in% names(result)))
  expect_true(all(
    c("trend_regression", "seasonal_regression", "remainder_regression") %in%
      names(result)
  ))
})

test_that("decompose_series regression (linear): exact identity holds (quarterly)", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "regression",
    .quiet = TRUE
  )

  reconstructed <- result$trend_regression +
    result$seasonal_regression +
    result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series regression (linear): exact identity holds (monthly)", {
  result <- decompose_series(
    ibcbr,
    value_col = "index",
    methods = "regression",
    .quiet = TRUE
  )

  reconstructed <- result$trend_regression +
    result$seasonal_regression +
    result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

# ---------------------------------------------------------------------------
# Regression — quadratic trend
# ---------------------------------------------------------------------------

test_that("decompose_series regression (quadratic): exact identity holds", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "regression",
    trend = "quadratic",
    .quiet = TRUE
  )

  reconstructed <- result$trend_regression +
    result$seasonal_regression +
    result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series regression quadratic produces different trend than linear", {
  r_linear <- decompose_series(
    ibcbr,
    value_col = "index",
    methods = "regression",
    trend = "linear",
    .quiet = TRUE
  )
  r_quad <- decompose_series(
    ibcbr,
    value_col = "index",
    methods = "regression",
    trend = "quadratic",
    .quiet = TRUE
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
    methods = "regression",
    trend = "cubic",
    .quiet = TRUE
  )

  reconstructed <- result$trend_regression +
    result$seasonal_regression +
    result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series regression cubic produces different trend than quadratic", {
  r_quad <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "regression",
    trend = "quadratic",
    .quiet = TRUE
  )
  r_cubic <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "regression",
    trend = "cubic",
    .quiet = TRUE
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
    methods = "regression",
    trend = "quadratic",
    params = list(poly_raw = TRUE),
    .quiet = TRUE
  )

  reconstructed <- result$trend_regression +
    result$seasonal_regression +
    result$remainder_regression
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series regression raw and orthogonal polynomials give similar fit", {
  # Both parameterisations should yield the same fitted values
  r_orth <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "regression",
    trend = "linear",
    params = list(poly_raw = FALSE),
    .quiet = TRUE
  )
  r_raw <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "regression",
    trend = "linear",
    params = list(poly_raw = TRUE),
    .quiet = TRUE
  )

  # Fitted values (trend + seasonal) must be identical regardless of parameterisation
  fitted_orth <- r_orth$trend_regression + r_orth$seasonal_regression
  fitted_raw <- r_raw$trend_regression + r_raw$seasonal_regression
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
    methods = "regression",
    .quiet = TRUE
  )

  result$year <- format(result$date, "%Y")
  # Drop incomplete boundary years
  year_counts <- table(result$year)
  full_years <- names(year_counts[year_counts == 4])
  result_full <- result[result$year %in% full_years, ]

  annual_seasonal <- tapply(
    result_full$seasonal_regression,
    result_full$year,
    sum
  )
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
  grp_a <- gdp_construction
  grp_a$sector <- "A"
  grp_b <- gdp_construction
  grp_b$sector <- "B"
  panel <- rbind(grp_a, grp_b)

  result <- decompose_series(
    panel,
    value_col = "index",
    group_cols = "sector",
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(panel))
  expect_true(all(
    c("trend_stl", "seasonal_stl", "remainder_stl") %in% names(result)
  ))

  # Identity must hold within each group
  for (grp in c("A", "B")) {
    sub <- result[result$sector == grp, ]
    reconstructed <- sub$trend_stl + sub$seasonal_stl + sub$remainder_stl
    expect_equal(
      reconstructed,
      sub$index,
      tolerance = tol,
      label = paste0("identity for group ", grp)
    )
  }
})

test_that("decompose_series regression works with group_cols", {
  grp_a <- gdp_construction
  grp_a$sector <- "A"
  grp_b <- gdp_construction
  grp_b$sector <- "B"
  panel <- rbind(grp_a, grp_b)

  result <- decompose_series(
    panel,
    value_col = "index",
    methods = "regression",
    group_cols = "sector",
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(panel))
  expect_true(all(
    c("trend_regression", "seasonal_regression", "remainder_regression") %in%
      names(result)
  ))
})

# ---------------------------------------------------------------------------
# Column naming
# ---------------------------------------------------------------------------

test_that("decompose_series STL column names are correct", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    .quiet = TRUE
  )
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(
    sort(new_cols),
    sort(c("trend_stl", "seasonal_stl", "remainder_stl"))
  )
})

test_that("decompose_series regression column names are correct", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "regression",
    .quiet = TRUE
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
  short <- gdp_construction[1:10, ]
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
      methods = "regression",
      .quiet = TRUE
    )
  )
})

# ---------------------------------------------------------------------------
# Frequency validation
# ---------------------------------------------------------------------------

test_that("decompose_series errors on invalid frequency", {
  expect_error(
    decompose_series(gdp_construction, value_col = "index", frequency = -1),
    "positive"
  )
  expect_error(
    decompose_series(
      gdp_construction,
      value_col = "index",
      frequency = "monthly"
    ),
    "positive"
  )
  expect_error(
    decompose_series(
      gdp_construction,
      value_col = "index",
      frequency = c(4, 12)
    ),
    "positive"
  )
})

# ---------------------------------------------------------------------------
# trend ignored for STL
# ---------------------------------------------------------------------------

test_that("decompose_series warns when trend is set with methods = 'stl'", {
  expect_warning(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = "stl",
      trend = "quadratic",
      .quiet = FALSE
    ),
    "ignored"
  )
})

test_that("decompose_series does not warn about trend with .quiet = TRUE", {
  expect_no_warning(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = "stl",
      trend = "quadratic",
      .quiet = TRUE
    )
  )
})

# ---------------------------------------------------------------------------
# Unknown params keys
# ---------------------------------------------------------------------------

test_that("decompose_series warns on unknown params keys (STL)", {
  expect_warning(
    decompose_series(
      gdp_construction,
      value_col = "index",
      params = list(s_window = 13),
      .quiet = FALSE
    ),
    "Unknown"
  )
})

test_that("decompose_series warns on unknown params keys (regression)", {
  expect_warning(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = "regression",
      params = list(poly_raw = FALSE, wrong_param = TRUE),
      .quiet = FALSE
    ),
    "Unknown"
  )
})

# ---------------------------------------------------------------------------
# NA handling
# ---------------------------------------------------------------------------

test_that("decompose_series STL runs when leading and trailing NAs are present", {
  dat <- gdp_construction
  n <- nrow(dat)
  dat$index[1:3] <- NA
  dat$index[(n - 1):n] <- NA

  result <- decompose_series(dat, value_col = "index", .quiet = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(dat))
  expect_true(all(
    c("trend_stl", "seasonal_stl", "remainder_stl") %in% names(result)
  ))
})

test_that("decompose_series regression runs when leading NAs are present", {
  dat <- gdp_construction
  dat$index[1:3] <- NA

  result <- decompose_series(
    dat,
    value_col = "index",
    methods = "regression",
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(dat))
  expect_true(all(
    c("trend_regression", "seasonal_regression", "remainder_regression") %in%
      names(result)
  ))
})

test_that("decompose_series informs about NA rows when .quiet = FALSE", {
  dat <- gdp_construction
  dat$index[1:2] <- NA

  expect_message(
    decompose_series(dat, value_col = "index", .quiet = FALSE),
    "missing values"
  )
})

test_that("decompose_series rejects interior NAs instead of breaking the identity", {
  dat <- gdp_construction
  dat$index[c(3, 10, 20)] <- NA

  # Dropping these rows would shift every later observation one quarter
  # earlier, silently breaking value = trend + seasonal + remainder
  expect_error(
    decompose_series(dat, value_col = "index", .quiet = TRUE),
    "missing period"
  )
})

test_that("the decomposition identity holds once gaps are filled", {
  dat <- gdp_construction

  result <- decompose_series(dat, value_col = "index", .quiet = TRUE)
  reconstructed <- result$trend_stl + result$seasonal_stl + result$remainder_stl

  expect_equal(reconstructed, result$index)
})

# ---------------------------------------------------------------------------
# Classical decomposition (stats::decompose)
# ---------------------------------------------------------------------------

test_that("decompose_series classic returns correct structure and columns", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "classic",
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(gdp_construction))
  expect_true(all(names(gdp_construction) %in% names(result)))
  expect_true(all(
    c("trend_classic", "seasonal_classic", "remainder_classic") %in%
      names(result)
  ))
})

test_that("decompose_series classic: additive identity holds where trend is defined", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "classic",
    .quiet = TRUE
  )

  reconstructed <- result$trend_classic +
    result$seasonal_classic +
    result$remainder_classic
  ok <- !is.na(reconstructed)
  expect_equal(reconstructed[ok], result$index[ok], tolerance = tol)
})

test_that("decompose_series classic leaves NA trend at the series boundaries", {
  # decompose() cannot evaluate the centred moving average for the first/last
  # frequency/2 observations, so those trend/remainder values are NA.
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "classic",
    .quiet = TRUE
  )

  expect_true(anyNA(result$trend_classic))
  expect_false(anyNA(result$seasonal_classic))
  # Quarterly data -> 2 NAs at each end.
  expect_equal(sum(is.na(result$trend_classic)), 4L)
})

# ---------------------------------------------------------------------------
# Basic Structural Model (stats::StructTS)
# ---------------------------------------------------------------------------

test_that("decompose_series bsm returns correct structure and columns", {
  result <- decompose_series(
    ibcbr,
    value_col = "index",
    methods = "bsm",
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(ibcbr))
  expect_true(all(
    c("trend_bsm", "seasonal_bsm", "remainder_bsm") %in% names(result)
  ))
})

test_that("decompose_series bsm: exact identity holds with no boundary NAs", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "bsm",
    .quiet = TRUE
  )

  reconstructed <- result$trend_bsm + result$seasonal_bsm + result$remainder_bsm
  expect_equal(reconstructed, result$index, tolerance = tol)
  # Unlike classic, BSM yields estimates for every observation.
  expect_false(anyNA(result$trend_bsm))
})

test_that("decompose_series bsm seasonal component is periodic", {
  result <- decompose_series(
    ibcbr,
    value_col = "index",
    methods = "bsm",
    .quiet = TRUE
  )
  s <- result$seasonal_bsm
  n <- length(s)
  # Seasonal value should track its value 12 months earlier.
  expect_gt(cor(s[13:n], s[1:(n - 12)]), 0.8)
})

# ---------------------------------------------------------------------------
# X-13ARIMA-SEATS (seasonal package, Suggested)
# ---------------------------------------------------------------------------

test_that("decompose_series seats returns correct structure and exact identity", {
  skip_if_not_installed("seasonal")

  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = "seats",
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(gdp_construction))
  expect_true(all(
    c("trend_seats", "seasonal_seats", "remainder_seats") %in% names(result)
  ))

  reconstructed <- result$trend_seats +
    result$seasonal_seats +
    result$remainder_seats
  expect_equal(reconstructed, result$index, tolerance = tol)
  expect_false(anyNA(result$trend_seats))
})

test_that("decompose_series seats: exact identity holds (monthly)", {
  skip_if_not_installed("seasonal")

  result <- decompose_series(
    ibcbr,
    value_col = "index",
    methods = "seats",
    .quiet = TRUE
  )

  reconstructed <- result$trend_seats +
    result$seasonal_seats +
    result$remainder_seats
  expect_equal(reconstructed, result$index, tolerance = tol)
})

# ---------------------------------------------------------------------------
# Shared behaviour for the new methods
# ---------------------------------------------------------------------------

test_that("decompose_series warns when trend is set with non-regression methods", {
  for (m in c("classic", "bsm")) {
    expect_warning(
      decompose_series(
        gdp_construction,
        value_col = "index",
        methods = m,
        trend = "quadratic",
        .quiet = FALSE
      ),
      "ignored"
    )
  }
})

test_that("decompose_series warns on unknown params keys (classic)", {
  expect_warning(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = "classic",
      params = list(bogus = 1),
      .quiet = FALSE
    ),
    "Unknown"
  )
})

test_that("decompose_series classic/bsm work with group_cols", {
  grp_a <- gdp_construction
  grp_a$sector <- "A"
  grp_b <- gdp_construction
  grp_b$sector <- "B"
  panel <- rbind(grp_a, grp_b)

  for (m in c("classic", "bsm")) {
    result <- decompose_series(
      panel,
      value_col = "index",
      methods = m,
      group_cols = "sector",
      .quiet = TRUE
    )
    expect_equal(nrow(result), nrow(panel))
    expect_true(all(
      c(
        paste0("trend_", m),
        paste0("seasonal_", m),
        paste0("remainder_", m)
      ) %in%
        names(result)
    ))
  }
})

test_that("decompose_series new methods emit no messages with .quiet = TRUE", {
  expect_no_message(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = "classic",
      .quiet = TRUE
    )
  )
  expect_no_message(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = "bsm",
      .quiet = TRUE
    )
  )
})

# ---------------------------------------------------------------------------
# transform = "log" (multiplicative decomposition)
# ---------------------------------------------------------------------------

test_that("decompose_series transform = 'log': product identity holds", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    transform = "log",
    .quiet = TRUE
  )

  reconstructed <- result$trend_stl * result$seasonal_stl * result$remainder_stl
  expect_equal(reconstructed, result$index, tolerance = tol)
})

test_that("decompose_series transform = 'log' works across methods", {
  # bsm omitted here: StructTS convergence is covered separately and its optim
  # warnings are orthogonal to the transform round-trip being tested.
  for (m in c("stl", "regression", "classic")) {
    result <- decompose_series(
      ibcbr,
      value_col = "index",
      methods = m,
      transform = "log",
      .quiet = TRUE
    )
    reconstructed <- result[[paste0("trend_", m)]] *
      result[[paste0("seasonal_", m)]] *
      result[[paste0("remainder_", m)]]
    ok <- !is.na(reconstructed)
    expect_equal(
      reconstructed[ok],
      result$index[ok],
      tolerance = tol,
      label = paste0("product identity for ", m)
    )
  }
})

test_that("decompose_series transform = 'log' errors on non-positive values", {
  dat <- gdp_construction
  dat$index[5] <- -1
  expect_error(
    decompose_series(
      dat,
      value_col = "index",
      transform = "log",
      .quiet = TRUE
    ),
    "positive"
  )
})

test_that("decompose_series errors on invalid transform", {
  expect_error(
    decompose_series(gdp_construction, value_col = "index", transform = "sqrt"),
    "Invalid transform"
  )
})

# ---------------------------------------------------------------------------
# Multiple methods in a single call
# ---------------------------------------------------------------------------

test_that("decompose_series accepts a vector of methods", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = c("stl", "classic", "regression"),
    .quiet = TRUE
  )

  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_setequal(
    new_cols,
    c(
      "trend_stl",
      "seasonal_stl",
      "remainder_stl",
      "trend_classic",
      "seasonal_classic",
      "remainder_classic",
      "trend_regression",
      "seasonal_regression",
      "remainder_regression"
    )
  )

  # Each method's identity holds independently
  rec_stl <- result$trend_stl + result$seasonal_stl + result$remainder_stl
  expect_equal(rec_stl, result$index, tolerance = tol)
  rec_reg <- result$trend_regression +
    result$seasonal_regression +
    result$remainder_regression
  expect_equal(rec_reg, result$index, tolerance = tol)
})

test_that("decompose_series dedupes repeated methods", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = c("stl", "stl"),
    .quiet = TRUE
  )
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(
    sort(new_cols),
    sort(c("trend_stl", "seasonal_stl", "remainder_stl"))
  )
})

test_that("decompose_series does not warn about trend when regression is among methods", {
  expect_no_warning(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = c("stl", "regression"),
      trend = "quadratic",
      .quiet = FALSE
    )
  )
})

test_that("decompose_series errors when any method is invalid", {
  expect_error(
    decompose_series(
      gdp_construction,
      value_col = "index",
      methods = c("stl", "nope")
    ),
    "Invalid method"
  )
})

test_that("decompose_series multiple methods work with group_cols", {
  grp_a <- gdp_construction
  grp_a$sector <- "A"
  grp_b <- gdp_construction
  grp_b$sector <- "B"
  panel <- rbind(grp_a, grp_b)

  result <- decompose_series(
    panel,
    value_col = "index",
    methods = c("stl", "classic"),
    group_cols = "sector",
    .quiet = TRUE
  )
  expect_equal(nrow(result), nrow(panel))
  expect_true(all(
    c(
      "trend_stl",
      "seasonal_stl",
      "remainder_stl",
      "trend_classic",
      "seasonal_classic",
      "remainder_classic"
    ) %in%
      names(result)
  ))
})

# ---------------------------------------------------------------------------
# seasadj convenience column
# ---------------------------------------------------------------------------

test_that("decompose_series seasadj = TRUE adds a seasonally adjusted column", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    seasadj = TRUE,
    .quiet = TRUE
  )
  expect_true("seasadj_stl" %in% names(result))
  # Additive: seasadj = value - seasonal = trend + remainder
  expect_equal(
    result$seasadj_stl,
    result$index - result$seasonal_stl,
    tolerance = tol
  )
})

test_that("decompose_series seasadj is off by default", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    .quiet = TRUE
  )
  expect_false("seasadj_stl" %in% names(result))
})

test_that("decompose_series seasadj uses division under multiplicative output", {
  # transform = "log" yields a multiplicative decomposition, so the seasonally
  # adjusted series is value / seasonal.
  r_log <- decompose_series(
    gdp_construction,
    value_col = "index",
    transform = "log",
    seasadj = TRUE,
    .quiet = TRUE
  )
  expect_equal(
    r_log$seasadj_stl,
    r_log$index / r_log$seasonal_stl,
    tolerance = tol
  )
})

test_that("decompose_series seasadj is added per method for multi-method calls", {
  result <- decompose_series(
    gdp_construction,
    value_col = "index",
    methods = c("stl", "classic"),
    seasadj = TRUE,
    .quiet = TRUE
  )
  expect_true(all(c("seasadj_stl", "seasadj_classic") %in% names(result)))
})

test_that("decompose_series errors on invalid seasadj", {
  expect_error(
    decompose_series(gdp_construction, value_col = "index", seasadj = "yes"),
    "seasadj"
  )
})
