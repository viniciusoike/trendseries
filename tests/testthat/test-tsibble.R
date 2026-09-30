monthly_tsibble <- function() {
  tsibble::as_tsibble(
    tibble::tibble(
      month = tsibble::yearmonth("2019 Jan") + 0:47,
      value = 100 + seq_len(48) + sin(seq_len(48))
    ),
    index = "month"
  )
}

test_that("tsibble trend results match the data-frame path", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  plain <- tibble::tibble(month = as.Date(input$month), value = input$value)

  result <- augment_trends(
    input,
    methods = c("hp", "ma"),
    window = 5,
    .quiet = TRUE
  )
  expected <- augment_trends(
    plain,
    date_col = "month",
    frequency = 12,
    methods = c("hp", "ma"),
    window = 5,
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_ts")
  expect_identical(tsibble::index_var(result), "month")
  expect_identical(tsibble::key_vars(result), character())
  expect_identical(result$month, input$month)
  expect_identical(result$value, input$value)
  expect_equal(result$trend_hp, expected$trend_hp)
  expect_equal(result$trend_ma, expected$trend_ma)
})

test_that("multiple tsibble keys keep trend results within each series", {
  skip_if_not_installed("tsibble")

  input <- tsibble::as_tsibble(
    tibble::tibble(
      quarter = rep(tsibble::yearquarter("2019 Q1") + 0:15, 4),
      region = rep(c("north", "south"), each = 32),
      sector = rep(rep(c("a", "b"), each = 16), 2),
      value = rep(seq_len(16), 4) + rep(c(10, 50, 100, 200), each = 16)
    ),
    index = "quarter",
    key = c(region, sector)
  )
  plain <- tibble::as_tibble(input)
  plain$quarter <- as.Date(plain$quarter)

  result <- augment_trends(input, methods = "ma", window = 3, .quiet = TRUE)
  expected <- augment_trends(
    plain,
    date_col = "quarter",
    group_cols = c("region", "sector"),
    frequency = 4,
    methods = "ma",
    window = 3,
    .quiet = TRUE
  )

  expect_identical(tsibble::key_vars(result), c("region", "sector"))
  expect_identical(result$quarter, input$quarter)
  expect_identical(result$region, input$region)
  expect_identical(result$sector, input$sector)
  expect_equal(result$trend_ma, expected$trend_ma)

  reordered_keys <- augment_trends(
    input,
    group_cols = c("sector", "region"),
    methods = "ma",
    window = 3,
    .quiet = TRUE
  )
  expect_equal(reordered_keys$trend_ma, expected$trend_ma)
  expect_identical(tsibble::key_vars(reordered_keys), c("region", "sector"))
})

test_that("tsibble detrending preserves metadata and additive identity", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  result <- detrend_series(
    input,
    methods = "ma",
    window = 5,
    components = TRUE,
    .quiet = TRUE
  )
  no_components <- detrend_series(
    input,
    methods = "ma",
    window = 5,
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_ts")
  expect_identical(result$month, input$month)
  observed <- !is.na(result$trend_ma)
  expect_equal(
    result$trend_ma[observed] + result$detrend_ma[observed],
    result$value[observed]
  )
  expect_s3_class(no_components, "tbl_ts")
  expect_false("trend_ma" %in% names(no_components))
  expect_equal(no_components$detrend_ma, result$detrend_ma)
})

test_that("tsibble log detrending and multiple values match the data-frame path", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  input$value2 <- input$value * 2
  plain <- tibble::as_tibble(input)
  plain$month <- as.Date(plain$month)

  result <- detrend_series(
    input,
    value_col = c("value", "value2"),
    methods = "hp",
    transform = "log",
    components = TRUE,
    .quiet = TRUE
  )
  expected <- detrend_series(
    plain,
    date_col = "month",
    value_col = c("value", "value2"),
    frequency = 12,
    methods = "hp",
    transform = "log",
    components = TRUE,
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_ts")
  expect_identical(result$month, input$month)
  for (column in c("value", "value2")) {
    trend <- paste0("trend_hp_", column)
    detrend <- paste0("detrend_hp_", column)
    expect_equal(result[[trend]], expected[[trend]])
    expect_equal(result[[detrend]], expected[[detrend]])
    expect_equal(result[[trend]] * exp(result[[detrend]]), input[[column]])
  }
})

test_that("keyed tsibble detrending does not mix groups", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  keyed <- tsibble::as_tsibble(
    tibble::tibble(
      month = rep(input$month, 2),
      group = rep(c("a", "b"), each = nrow(input)),
      value = c(input$value, input$value * 10)
    ),
    index = "month",
    key = "group"
  )
  plain <- tibble::as_tibble(keyed)
  plain$month <- as.Date(plain$month)

  result <- detrend_series(keyed, methods = "hp", .quiet = TRUE)
  expected <- detrend_series(
    plain,
    date_col = "month",
    group_cols = "group",
    frequency = 12,
    methods = "hp",
    .quiet = TRUE
  )

  expect_identical(tsibble::key_vars(result), "group")
  expect_equal(result$detrend_hp, expected$detrend_hp)
})

test_that("Date index, explicit metadata, and irregular flag are preserved", {
  skip_if_not_installed("tsibble")

  input <- tsibble::as_tsibble(
    tibble::tibble(
      period = seq.Date(as.Date("2019-01-01"), by = "month", length.out = 36),
      value = seq_len(36) + 100
    ),
    index = "period",
    regular = FALSE
  )

  result <- augment_trends(
    input,
    date_col = "period",
    frequency = 12,
    methods = "ma",
    window = 3,
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_ts")
  expect_s3_class(result$period, "Date")
  expect_identical(result$period, input$period)
  expect_false(tsibble::is_regular(result))
  expect_equal(result$trend_ma[3], mean(input$value[2:4]))

  detected <- augment_trends(input, methods = "hp", .quiet = TRUE)
  expect_s3_class(detected, "tbl_ts")
  expect_false(anyNA(detected$trend_hp))
})

test_that("missing values follow the existing trend rules", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  input$value[1] <- NA_real_
  result <- augment_trends(input, methods = "hp", .quiet = TRUE)
  expect_true(is.na(result$trend_hp[1]))
  expect_false(anyNA(result$trend_hp[-1]))

  input$value[12] <- NA_real_
  expect_error(
    augment_trends(input, methods = "hp", .quiet = TRUE),
    "missing|Missing"
  )
})

test_that("pre-existing trend names survive tsibble augmentation", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  input$trend_ma <- -999
  expect_warning(
    result <- augment_trends(input, methods = "ma", window = 3, .quiet = TRUE),
    "already exists"
  )

  expect_s3_class(result, "tbl_ts")
  expect_identical(result$trend_ma, input$trend_ma)
  expect_true("trend_ma_1" %in% names(result))
})

test_that("tsibble missing periods and unsupported indices fail clearly", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  missing_month <- input[-10, ]
  expect_error(
    augment_trends(missing_month, methods = "hp", .quiet = TRUE),
    "missing period"
  )

  weekly <- tsibble::as_tsibble(
    tibble::tibble(
      week = tsibble::yearweek(as.Date("2019-01-01") + 7 * 0:20),
      value = seq_len(21)
    ),
    index = "week"
  )
  expect_error(
    augment_trends(weekly, methods = "ma", window = 3, .quiet = TRUE),
    "Unsupported.*index"
  )
})

test_that("tsibble index and key overrides cannot conflict with metadata", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  input$other_date <- as.Date(input$month) + 1

  expect_error(
    augment_trends(
      input,
      date_col = "other_date",
      methods = "hp",
      .quiet = TRUE
    ),
    "date_col.*index"
  )

  keyed <- tsibble::as_tsibble(
    tibble::tibble(
      month = rep(input$month, 2),
      group = rep(c("a", "b"), each = nrow(input)),
      value = rep(input$value, 2)
    ),
    index = "month",
    key = "group"
  )
  expect_error(
    detrend_series(keyed, group_cols = NULL, .quiet = TRUE),
    "group_cols.*key"
  )
})

test_that("unordered tsibble input cannot silently reorder computed rows", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  reversed <- input[nrow(input):1, ]

  expect_error(
    augment_trends(reversed, methods = "ma", window = 3, .quiet = TRUE),
    "ordered"
  )
  expect_error(
    detrend_series(reversed, methods = "hp", .quiet = TRUE),
    "ordered"
  )
})

test_that("explicit empty tsibble keys match omitted keys", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()
  keys <- tsibble::key_vars(input)

  trend <- augment_trends(input, methods = "ma", window = 3, .quiet = TRUE)
  detrended <- detrend_series(input, methods = "ma", window = 3, .quiet = TRUE)

  expect_identical(
    augment_trends(
      input,
      group_cols = keys,
      methods = "ma",
      window = 3,
      .quiet = TRUE
    ),
    trend
  )
  expect_identical(
    detrend_series(
      input,
      group_cols = keys,
      methods = "ma",
      window = 3,
      .quiet = TRUE
    ),
    detrended
  )
})

test_that("tsibble augmentation preserves the input interval", {
  skip_if_not_installed("tsibble")

  input <- monthly_tsibble()[seq(1, 48, 3), ]
  expect_true(tsibble::has_gaps(input)$.gaps)

  for (result in list(
    augment_trends(
      input,
      frequency = 4,
      methods = "ma",
      window = 3,
      .quiet = TRUE
    ),
    detrend_series(
      input,
      frequency = 4,
      methods = "ma",
      window = 3,
      .quiet = TRUE
    )
  )) {
    expect_identical(tsibble::interval(result), tsibble::interval(input))
    expect_identical(tsibble::has_gaps(result), tsibble::has_gaps(input))
    expect_identical(result$month, input$month)
    expect_identical(result$value, input$value)
  }
})
