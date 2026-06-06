# Method registry (.method_info / .valid_methods) ----------------------------

test_that(".method_info() returns a well-formed registry", {
  info <- .method_info()
  expect_s3_class(info, "data.frame")
  expect_named(info, c("method", "category", "description"))
  expect_equal(nrow(info), 20L)
  expect_false(any(duplicated(info$method)))
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
