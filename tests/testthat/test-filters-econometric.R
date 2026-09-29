test_that("HP filter works correctly", {
  # Test with quarterly data
  ts_data <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)

  # Test basic functionality
  hp_trend <- extract_trends(ts_data, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_trend, "ts")
  expect_equal(length(hp_trend), length(ts_data))
  expect_false(any(is.na(hp_trend)))

  # Test default lambda for quarterly data (should be 1600)
  hp_default <- extract_trends(ts_data, methods = "hp", .quiet = TRUE)
  hp_1600 <- extract_trends(
    ts_data,
    methods = "hp",
    smoothing = 1600,
    .quiet = TRUE
  )
  expect_equal(as.numeric(hp_default), as.numeric(hp_1600), tolerance = 1e-10)

  # Test monthly data default (should be 129600, Ravn and Uhlig 2002)
  ts_monthly <- df_to_ts(vehicles, value_col = "production", frequency = 12)
  hp_monthly_default <- extract_trends(
    ts_monthly,
    methods = "hp",
    .quiet = TRUE
  )
  hp_monthly_129600 <- extract_trends(
    ts_monthly,
    methods = "hp",
    smoothing = 129600,
    .quiet = TRUE
  )
  expect_equal(
    as.numeric(hp_monthly_default),
    as.numeric(hp_monthly_129600),
    tolerance = 1e-10
  )
})

test_that("Baxter-King filter works correctly", {
  ts_data <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)

  # Test basic functionality
  bk_trend <- extract_trends(ts_data, methods = "bk", .quiet = TRUE)
  expect_s3_class(bk_trend, "ts")
  expect_equal(length(bk_trend), length(ts_data))

  # Should have NAs at endpoints due to bandpass nature
  expect_true(any(is.na(bk_trend)))

  # Test custom band parameters
  bk_custom <- extract_trends(
    ts_data,
    methods = "bk",
    band = c(8, 40),
    .quiet = TRUE
  )
  expect_s3_class(bk_custom, "ts")
  expect_false(identical(as.numeric(bk_trend), as.numeric(bk_custom)))
})

test_that("Christiano-Fitzgerald filter works correctly", {
  ts_data <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)

  # Test basic functionality
  cf_trend <- extract_trends(ts_data, methods = "cf", .quiet = TRUE)
  expect_s3_class(cf_trend, "ts")
  expect_equal(length(cf_trend), length(ts_data))

  # CF should handle endpoints better than BK (fewer NAs)
  bk_trend <- extract_trends(ts_data, methods = "bk", .quiet = TRUE)
  expect_true(sum(is.na(cf_trend)) <= sum(is.na(bk_trend)))
})

test_that("Hamilton filter works correctly", {
  # Convert data to ts object
  ts_data <- ts(gdp_construction$index, start = c(1996, 1), frequency = 4)

  # Test basic functionality
  hamilton_trend <- extract_trends(ts_data, methods = "hamilton", .quiet = TRUE)
  expect_s3_class(hamilton_trend, "ts")
  # Hamilton filter produces shorter series due to its regression nature
  expect_true(length(hamilton_trend) <= length(ts_data))

  # Test with short series (should fail)
  short_ts <- ts(1:10, frequency = 4)
  expect_error(
    extract_trends(short_ts, methods = "hamilton", .quiet = TRUE),
    "Time series too short"
  )

  # Test custom parameters
  hamilton_custom <- extract_trends(
    ts_data,
    methods = "hamilton",
    params = list(hamilton_h = 4, hamilton_p = 2),
    .quiet = TRUE
  )
  expect_s3_class(hamilton_custom, "ts")
})

test_that("Hamilton filter defaults are frequency-aware", {
  # Quarterly: h = 8, p = 4 -> first h + p - 1 = 11 observations are NA
  ts_q <- ts(gdp_construction$index, start = c(1996, 1), frequency = 4)
  trend_q <- extract_trends(ts_q, methods = "hamilton", .quiet = TRUE)
  expect_equal(sum(cumprod(is.na(trend_q))), 11)

  # Monthly: h = 24, p = 12 -> first h + p - 1 = 35 observations are NA
  ts_m <- ts(ibcbr$index, start = c(2003, 1), frequency = 12)
  trend_m <- extract_trends(ts_m, methods = "hamilton", .quiet = TRUE)
  expect_equal(sum(cumprod(is.na(trend_m))), 35)

  # Defaults match the explicit Hamilton (2018) monthly parameters
  trend_m_explicit <- extract_trends(
    ts_m,
    methods = "hamilton",
    params = list(hamilton_h = 24, hamilton_p = 12),
    .quiet = TRUE
  )
  expect_equal(as.numeric(trend_m), as.numeric(trend_m_explicit))
})

test_that("Beveridge-Nelson decomposition works", {
  # Convert data to ts object
  ts_data <- ts(gdp_construction$index, start = c(1996, 1), frequency = 4)

  # Test basic functionality
  bn_trend <- extract_trends(ts_data, methods = "bn", .quiet = TRUE)
  expect_s3_class(bn_trend, "ts")
  expect_equal(length(bn_trend), length(ts_data))
  # BN decomposition may have some NAs depending on AR model selection
  expect_true(sum(is.na(bn_trend)) < length(bn_trend) * 0.5) # Less than half should be NA
})

test_that("UCM returns the smoothed level of a fitted structural model", {
  ts_data <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)

  for (type in c("level", "trend", "BSM")) {
    expect_no_warning(
      ucm_trend <- extract_trends(
        ts_data,
        methods = "ucm",
        params = list(ucm_type = type),
        .quiet = TRUE
      )
    )
    expected <- stats::tsSmooth(stats::StructTS(ts_data, type = type))
    expect_equal(as.numeric(ucm_trend), as.numeric(expected[, "level"]))
    expect_equal(stats::tsp(ucm_trend), stats::tsp(ts_data))
  }
})

test_that("UCM defaults to BSM up to monthly data and level otherwise", {
  quarterly <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)
  expect_equal(
    extract_trends(quarterly, methods = "ucm", .quiet = TRUE),
    extract_trends(
      quarterly,
      methods = "ucm",
      params = list(ucm_type = "BSM"),
      .quiet = TRUE
    )
  )

  # BSM carries one state per season, which is impractical above monthly.
  set.seed(1)
  for (frequency in c(1, 52)) {
    series <- ts(cumsum(rnorm(3 * max(frequency, 20))), frequency = frequency)
    expect_equal(
      extract_trends(series, methods = "ucm", .quiet = TRUE),
      extract_trends(
        series,
        methods = "ucm",
        params = list(ucm_type = "level"),
        .quiet = TRUE
      )
    )
  }
})

test_that("UCM rejects high-frequency BSM before fitting", {
  local_mocked_bindings(
    StructTS = function(...) stop("fit was reached"),
    .package = "stats"
  )

  weekly <- stats::ts(seq_len(16), frequency = 52)
  expect_error(
    extract_trends(
      weekly,
      methods = "ucm",
      params = list(ucm_type = "BSM"),
      .quiet = TRUE
    ),
    "BSM.*frequency.*12"
  )
})

test_that("UCM ignores the unified smoothing parameter", {
  ts_data <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)

  alone <- extract_trends(ts_data, methods = "ucm", .quiet = TRUE)
  mixed <- extract_trends(
    ts_data,
    methods = c("hp", "ucm"),
    smoothing = 1600,
    .quiet = TRUE
  )

  expect_equal(mixed$ucm, alone)
})

test_that("Beveridge-Nelson uses bn_ar_order when supplied", {
  ts_data <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)

  auto <- extract_trends(ts_data, methods = "bn", .quiet = TRUE)
  ar1 <- extract_trends(
    ts_data,
    methods = "bn",
    params = list(bn_ar_order = 1),
    .quiet = TRUE
  )

  expect_equal(ar1, .beveridge_nelson_arima(ts_data, ar_order = 1))
  expect_false(isTRUE(all.equal(auto, ar1)))
})

test_that("Beveridge-Nelson order selection skips orders that fail to fit", {
  ts_data <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)
  arima <- stats::arima
  local_mocked_bindings(
    arima = function(x, order, ...) {
      if (order[1] == 1 && order[2] == 0) {
        stop("fit failed")
      }
      arima(x, order = order, ...)
    },
    .package = "stats"
  )

  # Before the fix, a failed fit left AIC = 0, so order 1 always won.
  result <- .beveridge_nelson_arima(ts_data)
  expect_false(isTRUE(all.equal(
    result,
    .beveridge_nelson_arima(ts_data, ar_order = 1)
  )))
})

test_that("Spencer filter works correctly", {
  # Test with quarterly data
  ts_data <- df_to_ts(gdp_construction, value_col = "index", frequency = 4)

  # Test basic functionality
  spencer_trend <- extract_trends(ts_data, methods = "spencer", .quiet = TRUE)
  expect_s3_class(spencer_trend, "ts")
  expect_equal(length(spencer_trend), length(ts_data))

  # Spencer uses linear extrapolation so should have no NAs
  expect_false(any(is.na(spencer_trend)))

  # Should be smoother than original (lower variance)
  expect_true(sd(spencer_trend, na.rm = TRUE) < sd(ts_data, na.rm = TRUE))

  # Test with monthly data
  ts_monthly <- df_to_ts(vehicles, value_col = "production", frequency = 12)
  spencer_monthly <- extract_trends(
    ts_monthly,
    methods = "spencer",
    .quiet = TRUE
  )
  expect_s3_class(spencer_monthly, "ts")
  expect_equal(length(spencer_monthly), length(ts_monthly))
  expect_false(any(is.na(spencer_monthly)))

  # Test minimum length requirement (should fail with < 15 obs)
  short_ts <- stats::ts(1:10, frequency = 4)
  expect_error(
    extract_trends(short_ts, methods = "spencer", .quiet = TRUE),
    "Spencer filter requires at least 15 observations"
  )
})

test_that(".default_band() spans 1.5 to 8 years in periods of the series", {
  expect_equal(.default_band(4), c(6, 32))
  expect_equal(.default_band(12), c(18, 96))
  expect_equal(.default_band(2), c(3, 16))
  # Baxter and King (1999) recommend 2 to 8 years for annual data
  expect_equal(.default_band(1), c(2, 8))
})

test_that("bandpass filters default to the band for the series' frequency", {
  monthly <- df_to_ts(ibcbr, value_col = "index", frequency = 12)

  for (method in c("bk", "cf")) {
    default_trend <- extract_trends(monthly, methods = method, .quiet = TRUE)
    explicit_trend <- extract_trends(
      monthly,
      methods = method,
      band = c(18, 96),
      .quiet = TRUE
    )
    expect_equal(default_trend, explicit_trend, label = method)
  }
})

test_that(".default_hp_lambda() scales 1600 by the fourth power of the frequency ratio", {
  expect_equal(.default_hp_lambda(1), 6.25)
  expect_equal(.default_hp_lambda(2), 100)
  expect_equal(.default_hp_lambda(4), 1600)
  expect_equal(.default_hp_lambda(12), 129600)
  expect_equal(.default_hp_lambda(52), 45697600)
})

test_that("HP defaults to lambda = 6.25 for annual data", {
  annual <- ts(cumsum(rnorm(60)), start = 1960, frequency = 1)
  default_trend <- extract_trends(annual, methods = "hp", .quiet = TRUE)
  explicit_trend <- extract_trends(
    annual,
    methods = "hp",
    smoothing = 6.25,
    .quiet = TRUE
  )
  expect_equal(default_trend, explicit_trend)

  # A fraction scales the frequency's default lambda
  half_trend <- extract_trends(
    annual,
    methods = "hp",
    smoothing = 0.5,
    .quiet = TRUE
  )
  explicit_half <- extract_trends(
    annual,
    methods = "hp",
    params = list(hp_lambda = 3.125),
    .quiet = TRUE
  )
  expect_equal(half_trend, explicit_half)
})

test_that("HP warns on weekly data with the default lambda, even when quiet", {
  weekly <- ts(cumsum(rnorm(200)), start = c(2020, 1), frequency = 52)

  expect_warning(
    extract_trends(weekly, methods = "hp", .quiet = TRUE),
    "45697600"
  )
  expect_no_warning(
    extract_trends(weekly, methods = "hp", smoothing = 1e5, .quiet = TRUE)
  )
  expect_no_warning(
    extract_trends(
      weekly,
      methods = "hp",
      params = list(hp_lambda = 1e5),
      .quiet = TRUE
    )
  )
})

test_that("HP does not warn on standard frequencies", {
  monthly <- df_to_ts(ibcbr, value_col = "index", frequency = 12)
  expect_no_warning(extract_trends(monthly, methods = "hp", .quiet = FALSE))
})
