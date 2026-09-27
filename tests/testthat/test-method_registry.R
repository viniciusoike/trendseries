# Method registry (.method_info / .valid_methods) ----------------------------

test_that(".method_info() returns a well-formed registry", {
  info <- .method_info()
  expect_s3_class(info, "data.frame")
  expect_named(
    info,
    c("method", "category", "description", "typical_use", "one_sided")
  )
  expect_equal(nrow(info), 20L)
  expect_false(any(duplicated(info$method)))
  expect_true(all(nzchar(info$description)))
  expect_true(all(nzchar(info$typical_use)))
  expect_in(info$one_sided, c("always", "option", "no"))
})

test_that(".valid_methods() matches the registry", {
  expect_setequal(.valid_methods(), .method_info()$method)
})

test_that("every registered method is accepted by extract_trends()", {
  # Guards against registry/validation drift: each listed method must pass the
  # valid_methods() check used inside extract_trends().
  expect_length(setdiff(.method_info()$method, .valid_methods()), 0)
})

test_that("all four documented categories are present", {
  cats <- unique(.method_info()$category)
  expect_setequal(
    cats,
    c("moving_average", "smoothing", "bandpass", "econometric")
  )
})

# Unified parameter table (.method_params) ----------------------------------

test_that(".method_params() covers every method in registry order", {
  params <- .method_params()
  expect_equal(params$method, .valid_methods())
  expect_named(
    params,
    c("method", "window", "window_vector", "smoothing", "band", "align")
  )
})

test_that("routing vectors only name registered methods", {
  routed <- c(
    .WINDOW_METHODS,
    .WINDOW_VECTOR_METHODS,
    .SMOOTHING_METHODS,
    .BAND_METHODS,
    .ALIGN_METHODS
  )
  expect_in(routed, .valid_methods())
  expect_in(.WINDOW_VECTOR_METHODS, .WINDOW_METHODS)
})

test_that(".method_params() agrees with .map_unified_params()", {
  # The vignette renders .method_params(); this checks each flag against what
  # the router actually does, so the table cannot drift from the code.
  params <- .method_params()
  routes <- function(method, ...) {
    mapped <- .map_unified_params(method, ..., frequency = 12)
    return(length(mapped) > 0)
  }

  for (i in seq_len(nrow(params))) {
    method <- params$method[i]
    expect_identical(
      routes(method, window = 5),
      params$window[i],
      label = method
    )
    expect_identical(
      routes(method, smoothing = 0.5),
      params$smoothing[i],
      label = method
    )
    expect_identical(
      routes(method, band = c(6, 32)),
      params$band[i],
      label = method
    )
    expect_identical(
      routes(method, align = "right"),
      params$align[i],
      label = method
    )
  }
})

test_that("methods with right alignment are marked as optionally one-sided", {
  info <- .method_info()
  aligned <- info$method %in% .ALIGN_METHODS
  expect_true(all(info$one_sided[aligned] == "option"))
})
