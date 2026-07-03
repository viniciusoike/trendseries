# Tests for deseason_series()
# Data used:
#   gdp_construction  — quarterly, value_col = "index"

tol <- 1e-10

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("deseason_series errors on invalid method", {
  expect_error(
    deseason_series(gdp_construction, value_col = "index", methods = "regression"),
    "Invalid method"
  )
  expect_error(
    deseason_series(gdp_construction, value_col = "index", methods = "hp"),
    "Invalid method"
  )
})

test_that("deseason_series errors on non-logical components", {
  expect_error(
    deseason_series(gdp_construction, value_col = "index", components = "yes"),
    "components"
  )
})

# Validation of shared arguments is delegated to decompose_series()
test_that("deseason_series delegates shared-argument validation", {
  expect_error(
    deseason_series(list(a = 1)),
    "data.frame"
  )
  expect_error(
    deseason_series(gdp_construction, value_col = "nonexistent"),
    "not found"
  )
})

# ---------------------------------------------------------------------------
# Default behaviour: only the seasonally adjusted column is added
# ---------------------------------------------------------------------------

test_that("deseason_series adds only seasadj column by default", {
  result <- deseason_series(gdp_construction, value_col = "index", .quiet = TRUE)
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(new_cols, "seasadj_stl")
  expect_false(any(c("trend_stl", "seasonal_stl", "remainder_stl") %in% names(result)))
  expect_true(is.numeric(result$seasadj_stl))
})

test_that("deseason_series seasadj matches trend + remainder (additive)", {
  full <- decompose_series(
    gdp_construction, value_col = "index", seasadj = TRUE, .quiet = TRUE
  )
  result <- deseason_series(gdp_construction, value_col = "index", .quiet = TRUE)
  expect_equal(result$seasadj_stl, full$seasadj_stl, tolerance = tol)
  expect_equal(
    result$seasadj_stl, full$trend_stl + full$remainder_stl, tolerance = tol
  )
})

# ---------------------------------------------------------------------------
# components = TRUE keeps the full decomposition
# ---------------------------------------------------------------------------

test_that("deseason_series with components = TRUE keeps all columns", {
  result <- deseason_series(
    gdp_construction, value_col = "index", components = TRUE, .quiet = TRUE
  )
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(
    sort(new_cols),
    sort(c("trend_stl", "seasonal_stl", "remainder_stl", "seasadj_stl"))
  )
  # Additive identity holds
  reconstructed <- result$trend_stl + result$seasonal_stl + result$remainder_stl
  expect_equal(reconstructed, result$index, tolerance = tol)
})

# ---------------------------------------------------------------------------
# Multiple methods
# ---------------------------------------------------------------------------

test_that("deseason_series supports multiple methods", {
  skip_if_not_installed("seasonal")
  result <- deseason_series(
    gdp_construction, value_col = "index", methods = c("stl", "seats"),
    .quiet = TRUE
  )
  new_cols <- setdiff(names(result), names(gdp_construction))
  expect_equal(sort(new_cols), sort(c("seasadj_stl", "seasadj_seats")))
})

# ---------------------------------------------------------------------------
# Log transform (multiplicative): seasadj matches trend * remainder
# ---------------------------------------------------------------------------

test_that("deseason_series log transform yields multiplicative seasadj", {
  full <- decompose_series(
    gdp_construction, value_col = "index", transform = "log",
    seasadj = TRUE, .quiet = TRUE
  )
  result <- deseason_series(
    gdp_construction, value_col = "index", transform = "log", .quiet = TRUE
  )
  expect_equal(result$seasadj_stl, full$seasadj_stl, tolerance = tol)
  expect_equal(
    result$seasadj_stl, full$trend_stl * full$remainder_stl, tolerance = tol
  )
})

# ---------------------------------------------------------------------------
# Grouped seasonal adjustment
# ---------------------------------------------------------------------------

test_that("deseason_series works with group_cols", {
  grp_a <- gdp_construction; grp_a$sector <- "A"
  grp_b <- gdp_construction; grp_b$sector <- "B"
  panel <- rbind(grp_a, grp_b)

  result <- deseason_series(
    panel, value_col = "index", group_cols = "sector", .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(panel))
  new_cols <- setdiff(names(result), names(panel))
  expect_equal(new_cols, "seasadj_stl")
  expect_false(anyNA(result$seasadj_stl))
})

# ---------------------------------------------------------------------------
# Pre-existing component-named columns are never dropped
# ---------------------------------------------------------------------------

test_that("deseason_series preserves a pre-existing component column", {
  df <- gdp_construction
  df$trend_stl <- 0
  expect_warning(
    result <- deseason_series(df, value_col = "index", .quiet = TRUE),
    "already exists"
  )
  # The user's column survives untouched; the renamed component is dropped
  expect_true("trend_stl" %in% names(result))
  expect_equal(unique(result$trend_stl), 0)
  expect_false("trend_stl_1" %in% names(result))
  expect_true("seasadj_stl" %in% names(result))
})

# ---------------------------------------------------------------------------
# .quiet suppresses messages
# ---------------------------------------------------------------------------

test_that("deseason_series with .quiet = TRUE emits no messages", {
  expect_silent(
    deseason_series(gdp_construction, value_col = "index", .quiet = TRUE)
  )
})
