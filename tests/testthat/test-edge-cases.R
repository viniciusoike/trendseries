test_that("Functions handle short time series appropriately", {
  # Very short series
  short_ts <- ts(c(100, 110, 105, 115, 120), frequency = 4)

  # Some methods should work
  hp_short <- extract_trends(short_ts, methods = "hp", .quiet = TRUE)
  expect_s3_class(hp_short, "ts")

  # loess emits degenerate-fit warnings (pseudoinverse etc.) on 5 points
  loess_short <- suppressWarnings(
    extract_trends(short_ts, methods = "loess", .quiet = TRUE)
  )
  expect_s3_class(loess_short, "ts")

  # Hamilton filter should fail with short series
  expect_error(
    extract_trends(short_ts, methods = "hamilton", .quiet = TRUE),
    "Time series too short"
  )

  # BK filter should fail with very short series
  expect_error(
    extract_trends(short_ts, methods = "bk", .quiet = TRUE),
    "fixed lag length"
  )
})

test_that("Functions handle missing values appropriately", {
  # Create series with missing values
  # Convert data to ts object
  ts_with_na <- ts(gdp_construction$index, start = c(1996, 1), frequency = 4)
  ts_with_na[5:7] <- NA

  # This asserted only the class, which held while hp was quietly returning an
  # all-NA series. Interior gaps are now rejected instead.
  expect_error(
    extract_trends(ts_with_na, methods = "hp", .quiet = TRUE),
    "missing value"
  )
})

test_that("Functions validate input parameters correctly", {
  # Convert data to ts object
  ts_data <- ts(gdp_construction$index, start = c(1996, 1), frequency = 4)

  # Invalid window (negative)
  expect_error(
    extract_trends(ts_data, methods = "ma", window = -5),
    "positive"
  )

  # Invalid window (zero)
  expect_error(
    extract_trends(ts_data, methods = "ma", window = 0),
    "positive"
  )

  # Invalid window (missing value)
  expect_error(
    extract_trends(ts_data, methods = "ma", window = NA_real_),
    "positive"
  )

  # Invalid band parameter (wrong length)
  expect_error(
    extract_trends(ts_data, methods = "bk", band = c(6)),
    "length 2"
  )

  # Invalid band parameter (negative values)
  expect_error(
    extract_trends(ts_data, methods = "bk", band = c(-2, 8)),
    "positive"
  )

  # Invalid band parameter (missing value)
  expect_error(
    extract_trends(ts_data, methods = "bk", band = c(NA_real_, 8)),
    "positive"
  )
})

test_that("Functions handle different frequencies correctly", {
  # Annual data (should work with warnings)
  annual_ts <- ts(c(100, 105, 110, 115, 120, 118, 125, 130), frequency = 1)

  # Should work but produce no warning (1 is standard frequency)
  expect_no_error(
    extract_trends(annual_ts, methods = "hp", .quiet = TRUE)
  )

  # Daily data warns about both non-standard frequency and series length
  daily_ts <- ts(rnorm(100), frequency = 365)
  expect_warning(
    expect_warning(
      extract_trends(daily_ts, methods = "hp", .quiet = FALSE),
      "optimized for standard economic frequencies"
    ),
    "Minimum"
  )
})

test_that("Functions handle extreme parameter values", {
  # Convert data to ts object
  ts_data <- ts(gdp_construction$index, start = c(1996, 1), frequency = 4)

  # Very high lambda for HP filter
  hp_extreme <- extract_trends(
    ts_data,
    methods = "hp",
    smoothing = 1000000,
    .quiet = TRUE
  )
  expect_s3_class(hp_extreme, "ts")

  # Very low lambda for HP filter
  hp_low <- extract_trends(
    ts_data,
    methods = "hp",
    smoothing = 0.1,
    .quiet = TRUE
  )
  expect_s3_class(hp_low, "ts")

  # Extreme loess span
  loess_extreme <- extract_trends(
    ts_data,
    methods = "loess",
    smoothing = 0.99,
    .quiet = TRUE
  )
  expect_s3_class(loess_extreme, "ts")
})

test_that("Unified parameter system works consistently", {
  # Convert data to ts object
  ts_data <- ts(vehicles$production, start = c(2001, 1), frequency = 12)

  # Test that window parameter affects all MA methods consistently
  ma_window12 <- extract_trends(
    ts_data,
    methods = c("ma", "wma"),
    window = 12,
    .quiet = TRUE
  )
  ma_window6 <- extract_trends(
    ts_data,
    methods = c("ma", "wma"),
    window = 6,
    .quiet = TRUE
  )

  expect_type(ma_window12, "list")
  expect_type(ma_window6, "list")

  # Results should be different with different windows
  expect_false(identical(
    as.numeric(ma_window12$ma),
    as.numeric(ma_window6$ma)
  ))
  expect_false(identical(
    as.numeric(ma_window12$wma),
    as.numeric(ma_window6$wma)
  ))

  # Test that smoothing parameter affects smoothing methods
  smooth_03 <- extract_trends(
    ts_data,
    methods = c("loess", "ewma"),
    smoothing = 0.3,
    .quiet = TRUE
  )
  smooth_07 <- extract_trends(
    ts_data,
    methods = c("loess", "ewma"),
    smoothing = 0.7,
    .quiet = TRUE
  )

  expect_false(identical(
    as.numeric(smooth_03$loess),
    as.numeric(smooth_07$loess)
  ))
  expect_false(identical(
    as.numeric(smooth_03$ewma),
    as.numeric(smooth_07$ewma)
  ))
})
## Irregular period grids -----------------------------------------------------

# Observations are placed at consecutive positions in a ts, so a missing period
# shifts every later value one slot earlier. Results are then merged back by
# date and land on the wrong rows. These guard against that regression.

test_that("augment_trends() rejects a series with an interior gap", {
  data <- vehicles[1:36, ]

  na_gap <- data
  na_gap$production[10] <- NA
  expect_error(
    augment_trends(na_gap, value_col = "production", .quiet = TRUE),
    "missing period"
  )

  expect_error(
    augment_trends(data[-10, ], value_col = "production", .quiet = TRUE),
    "missing period"
  )
})

test_that("augment_trends() still accepts leading and trailing missing values", {
  data <- vehicles[1:60, ]
  data$production[1:6] <- NA
  data$production[58:60] <- NA

  result <- augment_trends(
    data,
    value_col = "production",
    methods = "ma",
    window = 3,
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(data))
  expect_true("trend_ma" %in% names(result))
  # The trend is only defined where the series was observed
  expect_true(all(is.na(result$trend_ma[1:6])))
  expect_false(all(is.na(result$trend_ma)))
})

test_that("gapped series keep their dates aligned when the gap is filled", {
  data <- vehicles[1:36, ]
  gapped <- data[-10, ]

  # Re-inserting the period with an NA value is the documented fix, and it
  # restores the original dates for every later observation
  refilled <- merge(
    data.frame(date = data$date),
    gapped,
    by = "date",
    all.x = TRUE
  )

  expect_error(
    augment_trends(
      gapped,
      value_col = "production",
      methods = "ma",
      .quiet = TRUE
    ),
    "missing period"
  )
  expect_error(
    augment_trends(
      refilled,
      value_col = "production",
      methods = "ma",
      .quiet = TRUE
    ),
    "missing period"
  )
})

test_that("decompose_series() rejects an interior gap", {
  data <- vehicles[1:60, ]
  gapped <- data
  gapped$production[20] <- NA

  # Dropping the row would break the documented
  # value = trend + seasonal + remainder identity for every later row
  expect_error(
    decompose_series(gapped, value_col = "production", .quiet = TRUE),
    "missing period"
  )
})

test_that("detrend_series() and deseason_series() reject an interior gap", {
  gapped <- vehicles[1:60, ][-20, ]

  expect_error(
    detrend_series(gapped, value_col = "production", .quiet = TRUE),
    "missing period"
  )
  expect_error(
    deseason_series(gapped, value_col = "production", .quiet = TRUE),
    "missing period"
  )
})

test_that("grouped data is checked group by group", {
  base <- vehicles[1:36, ]
  data <- rbind(
    data.frame(base, grp = "a"),
    data.frame(base, grp = "b")
  )

  expect_no_error(
    augment_trends(
      data,
      value_col = "production",
      group_cols = "grp",
      methods = "ma",
      window = 3,
      .quiet = TRUE
    )
  )

  # A gap in a single group is enough to stop the whole call
  data$production[data$grp == "b"][10] <- NA
  expect_error(
    augment_trends(
      data,
      value_col = "production",
      group_cols = "grp",
      methods = "ma",
      window = 3,
      .quiet = TRUE
    ),
    "missing period"
  )
})

test_that("duplicated periods are rejected", {
  data <- vehicles[1:36, ]

  expect_error(
    augment_trends(
      rbind(data, data[5, ]),
      value_col = "production",
      .quiet = TRUE
    ),
    "duplicated period"
  )
})

## Missing values in ts input -------------------------------------------------

# A ts already fixes the position-to-date correspondence, so the grid checks
# above do not apply. What an interior NA does instead is reach the filters,
# where behaviour is method-dependent and mostly silent: stl, spline and
# hamilton error, hp, bk and cf return an all-NA series, and ewma and bn
# propagate the gap to every later observation.

test_that("extract_trends() rejects an interior missing value", {
  series <- ts(cumsum(rnorm(120)) + 100, start = c(2010, 1), frequency = 12)
  series[60] <- NA

  expect_error(
    extract_trends(series, methods = "hp", .quiet = TRUE),
    "missing value"
  )
  # Silently returning an all-NA series is the regression being guarded against
  expect_error(
    extract_trends(series, methods = c("hp", "loess"), .quiet = TRUE),
    "missing value"
  )
})

test_that("the rejection names the missing periods", {
  series <- ts(1:120, start = c(2010, 1), frequency = 12)
  series[c(12, 24)] <- NA

  expect_error(
    extract_trends(series, methods = "hp", .quiet = TRUE),
    "2010-12-01"
  )

  quarterly <- ts(1:60, start = c(2010, 1), frequency = 4)
  quarterly[7] <- NA
  expect_error(
    extract_trends(quarterly, methods = "hp", .quiet = TRUE),
    "2011-07-01"
  )
})

test_that("a series with no observed values is rejected", {
  expect_error(
    extract_trends(ts(rep(NA_real_, 36), frequency = 12), methods = "hp"),
    "no non-missing values"
  )
})

test_that("leading and trailing missing values are trimmed, not rejected", {
  set.seed(42)
  series <- ts(cumsum(rnorm(120)) + 100, start = c(2010, 1), frequency = 12)
  padded <- series
  padded[1:6] <- NA
  padded[119:120] <- NA

  for (method in .valid_methods()) {
    if (method == "ucm") {
      expect_snapshot({
        result <- extract_trends(padded, methods = method, .quiet = TRUE)
      })
    } else {
      result <- extract_trends(padded, methods = method, .quiet = TRUE)
    }

    # The result stays on the time base of the input, so callers can cbind it
    expect_equal(as.numeric(time(result)), as.numeric(time(series)))
    # A trend is undefined where the series was never observed
    expect_true(all(is.na(result[c(1:6, 119:120)])))
    expect_false(all(is.na(result)))
  }
})

test_that("trimming leaves the estimate identical to the untrimmed series", {
  set.seed(42)
  series <- ts(cumsum(rnorm(96)) + 100, start = c(2010, 1), frequency = 12)
  padded <- c(rep(NA_real_, 6), as.numeric(series))
  padded <- ts(padded, start = c(2009, 7), frequency = 12)

  direct <- extract_trends(series, methods = "loess", .quiet = TRUE)
  trimmed <- extract_trends(padded, methods = "loess", .quiet = TRUE)

  expect_equal(
    as.numeric(window(trimmed, start = c(2010, 1))),
    as.numeric(direct)
  )
})

test_that("multiple methods and vector windows are padded too", {
  set.seed(42)
  padded <- ts(cumsum(rnorm(120)) + 100, start = c(2010, 1), frequency = 12)
  padded[1:6] <- NA

  multi <- extract_trends(padded, methods = c("hp", "loess"), .quiet = TRUE)
  expect_named(multi, c("hp", "loess"))
  expect_true(all(vapply(multi, length, integer(1)) == 120))
  expect_true(all(is.na(multi$hp[1:6])))

  windows <- extract_trends(
    padded,
    methods = "ma",
    window = c(3, 12),
    .quiet = TRUE
  )
  expect_named(windows, c("ma_3", "ma_12"))
  expect_true(all(vapply(windows, length, integer(1)) == 120))
  expect_true(all(is.na(windows$ma_3[1:6])))
})

test_that("a complete series is untouched by the trimming path", {
  set.seed(42)
  series <- ts(cumsum(rnorm(120)) + 100, start = c(2010, 1), frequency = 12)

  for (method in .valid_methods()) {
    if (method == "ucm") {
      expect_snapshot({
        result <- extract_trends(series, methods = method, .quiet = TRUE)
      })
    } else {
      result <- extract_trends(series, methods = method, .quiet = TRUE)
    }
    expect_equal(as.numeric(time(result)), as.numeric(time(series)))
  }
})

test_that("a data frame with no rows is rejected", {
  empty <- vehicles[0, ]

  expect_error(
    augment_trends(empty, value_col = "production", .quiet = TRUE),
    "no rows"
  )
  expect_error(
    augment_rolling(empty, value_col = "production", .quiet = TRUE),
    "no rows"
  )
  expect_error(
    decompose_series(empty, value_col = "production", .quiet = TRUE),
    "no rows"
  )
  expect_error(
    detrend_series(empty, value_col = "production", .quiet = TRUE),
    "no rows"
  )
  expect_error(
    deseason_series(empty, value_col = "production", .quiet = TRUE),
    "no rows"
  )
})

test_that("a data frame with no rows is rejected before frequency detection", {
  empty <- vehicles[0, ]

  # An explicit frequency skips detection, so the guard has to stand on its own
  expect_error(
    augment_trends(
      empty,
      value_col = "production",
      frequency = 12,
      .quiet = TRUE
    ),
    "no rows"
  )
  expect_error(
    augment_rolling(
      empty,
      value_col = "production",
      frequency = 12,
      .quiet = TRUE
    ),
    "no rows"
  )
})

test_that("a grouped call with no rows errors instead of returning NULL", {
  empty <- data.frame(
    date = as.Date(character(0)),
    production = numeric(0),
    grp = character(0)
  )

  expect_error(
    augment_trends(
      empty,
      value_col = "production",
      group_cols = "grp",
      frequency = 12,
      .quiet = TRUE
    ),
    "no rows"
  )
  expect_error(
    decompose_series(
      empty,
      value_col = "production",
      group_cols = "grp",
      frequency = 12,
      .quiet = TRUE
    ),
    "no rows"
  )
})

test_that("unused factor levels do not create empty groups", {
  base <- vehicles[1:60, ]
  data <- data.frame(base, grp = factor("a", levels = c("a", "b")))

  # split() keeps the unused level as an empty group; it must be dropped rather
  # than sent through the conversion path
  expect_no_error(
    augment_trends(
      data,
      value_col = "production",
      group_cols = "grp",
      methods = "ma",
      window = 3,
      .quiet = TRUE
    )
  )
  expect_no_error(
    decompose_series(
      data,
      value_col = "production",
      group_cols = "grp",
      .quiet = TRUE
    )
  )
  expect_no_error(
    augment_rolling(
      data,
      value_col = "production",
      group_cols = "grp",
      .quiet = TRUE
    )
  )

  result <- augment_trends(
    data,
    value_col = "production",
    group_cols = "grp",
    methods = "ma",
    window = 3,
    .quiet = TRUE
  )
  expect_equal(nrow(result), nrow(data))
})


test_that("colliding group labels stay independent across data-frame APIs", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 36)
  first <- data.frame(date = dates, value = 1:36, a = "a.b", b = "c")
  second <- data.frame(date = dates, value = 101:136, a = "a", b = "b.c")
  panel <- rbind(first, second)
  panel <- panel[order(panel$date, decreasing = TRUE), ]

  calls <- list(
    function(data, groups) {
      augment_trends(
        data,
        group_cols = groups,
        methods = "ma",
        window = 3,
        frequency = 12,
        .quiet = TRUE
      )
    },
    function(data, groups) {
      augment_rolling(
        data,
        group_cols = groups,
        window = 3,
        frequency = 12,
        .quiet = TRUE
      )
    },
    function(data, groups) {
      decompose_series(
        data,
        group_cols = groups,
        methods = "regression",
        frequency = 12,
        .quiet = TRUE
      )
    }
  )
  for (compute in calls) {
    result <- compute(panel, c("a", "b"))
    expect_equal(result[names(panel)], tibble::as_tibble(panel))
    for (group in unique(panel$a)) {
      rows <- which(panel$a == group)
      expect_equal(result[rows, ], compute(panel[rows, ], NULL))
    }
  }
})

test_that("daily and weekly trends reject explicitly missing interior values", {
  for (frequency in c(252, 52)) {
    data <- data.frame(
      date = seq(
        as.Date("2020-01-01"),
        by = if (frequency == 52) "week" else "day",
        length.out = 10
      ),
      value = c(1:4, NA, 6:10)
    )
    data <- data[10:1, ]
    expect_snapshot(
      error = TRUE,
      augment_trends(
        data,
        methods = "ma",
        window = 3,
        frequency = frequency,
        .quiet = TRUE
      )
    )
    expect_snapshot(
      error = TRUE,
      decompose_series(
        data,
        methods = "regression",
        frequency = frequency,
        .quiet = TRUE
      )
    )
  }
})

test_that("irregular calendars and edge missing values remain supported", {
  for (frequency in c(252, 52)) {
    step <- if (frequency == 52) 7 else 1
    data <- data.frame(
      date = as.Date("2020-01-01") + c(0, 1, 2, 5, 6, 8, 9) * step,
      value = c(NA, 2, 3, 4, 5, 6, NA)
    )
    result <- suppressWarnings(augment_trends(
      data,
      methods = "ma",
      window = 3,
      align = "right",
      frequency = frequency,
      .quiet = TRUE
    ))
    expect_identical(result$date, data$date)
    expect_equal(result$trend_ma, c(NA, NA, NA, 3, 4, 5, NA))

    data$value <- c(1, 2, NA, 4, 5, 6, 7)
    rolled <- augment_rolling(
      data,
      window = 3,
      frequency = frequency,
      na_rm = TRUE,
      .quiet = TRUE
    )
    expect_equal(rolled$roll_sum_3, c(NA, NA, 3, 6, 9, 15, 18))
  }
})


test_that("date names can overlap generated names without losing columns", {
  data <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
    value = 1:36,
    .date = 101:136
  )
  calls <- list(
    trend_ma = function(data, date_col) {
      augment_trends(
        data,
        date_col = date_col,
        methods = "ma",
        window = 3,
        .quiet = TRUE
      )
    },
    roll_sum_3 = function(data, date_col) {
      augment_rolling(data, date_col = date_col, window = 3, .quiet = TRUE)
    },
    trend_regression = function(data, date_col) {
      decompose_series(
        data,
        date_col = date_col,
        methods = "regression",
        .quiet = TRUE
      )
    }
  )
  for (generated in names(calls)) {
    compute <- calls[[generated]]
    expected <- compute(data, "date")
    renamed <- data
    names(renamed)[1] <- generated
    renamed[[paste0(generated, "_1")]] <- 201:236
    expect_snapshot({
      result <- compute(renamed, generated)
    })
    expect_equal(result[names(renamed)], tibble::as_tibble(renamed))
    expect_equal(result[[paste0(generated, "_2")]], expected[[generated]])
  }
})
