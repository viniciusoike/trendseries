## Basic behaviour -----------------------------------------------------------

test_that("augment_rolling() adds a roll_ column without altering the input", {
  result <- augment_rolling(
    vehicles,
    value_col = "production",
    window = 12,
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(vehicles))
  expect_true(all(names(vehicles) %in% names(result)))
  expect_equal(setdiff(names(result), names(vehicles)), "roll_sum_12")
  expect_type(result$roll_sum_12, "double")
})

test_that("column names encode both the statistic and the window", {
  result <- augment_rolling(
    vehicles,
    value_col = "production",
    stats = c("sum", "mean"),
    window = c(3, 12),
    .quiet = TRUE
  )

  expect_setequal(
    setdiff(names(result), names(vehicles)),
    c("roll_sum_3", "roll_mean_3", "roll_sum_12", "roll_mean_12")
  )
})

test_that("year-to-date columns are named roll_{stat}_ytd", {
  result <- suppressWarnings(augment_rolling(
    vehicles,
    value_col = "production",
    window = "ytd",
    .quiet = TRUE
  ))

  expect_equal(setdiff(names(result), names(vehicles)), "roll_sum_ytd")
})

test_that("suffix is appended to the generated names", {
  result <- augment_rolling(
    vehicles,
    value_col = "production",
    window = 12,
    suffix = "veh",
    .quiet = TRUE
  )

  expect_equal(setdiff(names(result), names(vehicles)), "roll_sum_12_veh")
})

test_that("window defaults to the detected frequency", {
  monthly <- augment_rolling(vehicles, value_col = "production", .quiet = TRUE)
  expect_true("roll_sum_12" %in% names(monthly))

  quarterly <- augment_rolling(
    gdp_construction,
    value_col = "index",
    .quiet = TRUE
  )
  expect_true("roll_sum_4" %in% names(quarterly))
})

## Values agree with the ts interface -----------------------------------------

test_that("augmented values match roll_series() on the same series", {
  # Production levels are not rates, so the chain scale guard fires on both
  augmented <- suppressWarnings(augment_rolling(
    vehicles,
    value_col = "production",
    stats = c("sum", "chain"),
    window = 12,
    .quiet = TRUE
  ))

  ts_data <- df_to_ts(vehicles, value_col = "production", frequency = 12)
  expected <- suppressWarnings(roll_series(
    ts_data,
    stats = c("sum", "chain"),
    window = 12,
    .quiet = TRUE
  ))

  augmented <- augmented[order(augmented$date), ]
  expect_equal(augmented$roll_sum_12, as.numeric(expected$sum_12))
  expect_equal(augmented$roll_chain_12, as.numeric(expected$chain_12))
})

test_that("the 12-month sum equals the sum of the trailing 12 observations", {
  result <- augment_rolling(
    vehicles,
    value_col = "production",
    window = 12,
    .quiet = TRUE
  )
  result <- result[order(result$date), ]

  expect_true(all(is.na(result$roll_sum_12[1:11])))
  expect_equal(result$roll_sum_12[12], sum(vehicles$production[1:12]))
  expect_equal(result$roll_sum_12[30], sum(vehicles$production[19:30]))
})

## Multiple value columns -----------------------------------------------------

test_that("several value columns each get their own suffixed column", {
  data <- data.frame(
    date = vehicles$date,
    a = vehicles$production,
    b = vehicles$production * 2
  )

  result <- augment_rolling(
    data,
    value_col = c("a", "b"),
    window = 4,
    .quiet = TRUE
  )

  expect_setequal(
    setdiff(names(result), names(data)),
    c("roll_sum_4_a", "roll_sum_4_b")
  )
  expect_equal(result$roll_sum_4_b, result$roll_sum_4_a * 2)
})

## Grouping -------------------------------------------------------------------

test_that("grouped results match per-group computation and preserve row count", {
  grouped <- augment_rolling(
    retail_volume,
    group_cols = "name_series",
    window = 12,
    .quiet = TRUE
  )

  expect_equal(nrow(grouped), nrow(retail_volume))
  expect_true("roll_sum_12" %in% names(grouped))

  target <- unique(retail_volume$name_series)[1]
  solo <- augment_rolling(
    retail_volume[retail_volume$name_series == target, ],
    window = 12,
    .quiet = TRUE
  )
  from_grouped <- grouped[grouped$name_series == target, ]

  expect_equal(
    from_grouped$roll_sum_12[order(from_grouped$date)],
    solo$roll_sum_12[order(solo$date)]
  )
})

test_that("windows do not bleed across groups", {
  data <- rbind(
    data.frame(
      date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
      value = rep(1, 12),
      grp = "a"
    ),
    data.frame(
      date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
      value = rep(10, 12),
      grp = "b"
    )
  )

  result <- augment_rolling(
    data,
    group_cols = "grp",
    window = 3,
    frequency = 12,
    .quiet = TRUE
  )

  a <- result[result$grp == "a", ]
  b <- result[result$grp == "b", ]

  expect_true(all(is.na(a$roll_sum_3[1:2])))
  expect_true(all(a$roll_sum_3[3:12] == 3))
  expect_true(all(is.na(b$roll_sum_3[1:2])))
  expect_true(all(b$roll_sum_3[3:12] == 30))
})

test_that("ytd resets per group", {
  data <- data.frame(
    date = rep(seq(as.Date("2020-11-01"), by = "month", length.out = 4), 2),
    value = rep(1, 8),
    grp = rep(c("a", "b"), each = 4)
  )

  result <- suppressWarnings(augment_rolling(
    data,
    group_cols = "grp",
    window = "ytd",
    frequency = 12,
    .quiet = TRUE
  ))

  for (g in c("a", "b")) {
    rows <- result[result$grp == g, ]
    rows <- rows[order(rows$date), ]
    expect_equal(rows$roll_sum_ytd, c(1, 2, 1, 2))
  }
})

## Missing values -------------------------------------------------------------

test_that("rows with missing values keep their calendar position", {
  data <- vehicles[1:24, ]
  data$production[10] <- NA

  result <- augment_rolling(
    data,
    value_col = "production",
    window = 3,
    .quiet = TRUE
  )
  result <- result[order(result$date), ]

  # The dates are untouched and the gap propagates to the three windows
  # that cover it, rather than shifting later observations
  expect_equal(result$date, data$date)
  expect_true(all(is.na(result$roll_sum_3[10:12])))
  expect_equal(result$roll_sum_3[13], sum(data$production[11:13]))
})

test_that("na_rm = TRUE fills windows that span a missing value", {
  data <- vehicles[1:24, ]
  data$production[10] <- NA

  result <- augment_rolling(
    data,
    value_col = "production",
    window = 3,
    na_rm = TRUE,
    .quiet = TRUE
  )
  result <- result[order(result$date), ]

  expect_equal(result$roll_sum_3[10], sum(data$production[8:9]))
  expect_equal(result$roll_sum_3[13], sum(data$production[11:13]))
})

## Naming conflicts -----------------------------------------------------------

test_that("existing columns are not overwritten", {
  data <- vehicles
  data$roll_sum_12 <- 0

  expect_warning(
    result <- augment_rolling(
      data,
      value_col = "production",
      window = 12,
      .quiet = TRUE
    ),
    "already exists"
  )

  expect_true("roll_sum_12_1" %in% names(result))
  expect_true(all(result$roll_sum_12 == 0))
})

## Validation -----------------------------------------------------------------

test_that("data frame inputs are validated", {
  expect_error(
    augment_rolling(list(a = 1), .quiet = TRUE),
    "must be a data.frame"
  )
  expect_error(
    augment_rolling(vehicles, date_col = "nope", .quiet = TRUE),
    "not found in data"
  )
  expect_error(
    augment_rolling(vehicles, value_col = "nope", .quiet = TRUE),
    "not found in data"
  )
  expect_error(
    augment_rolling(
      vehicles,
      value_col = "production",
      group_cols = "nope",
      .quiet = TRUE
    ),
    "Group variables not found"
  )
})

test_that("date and value column types are checked", {
  bad_date <- vehicles
  bad_date$date <- as.character(bad_date$date)
  expect_error(
    augment_rolling(bad_date, value_col = "production", .quiet = TRUE),
    "must be of class Date"
  )

  bad_value <- vehicles
  bad_value$production <- as.character(bad_value$production)
  expect_error(
    augment_rolling(bad_value, value_col = "production", .quiet = TRUE),
    "must be numeric"
  )
})

test_that("rolling arguments are validated through the data frame interface", {
  expect_error(
    augment_rolling(
      vehicles,
      value_col = "production",
      stats = "bogus",
      .quiet = TRUE
    ),
    "Invalid rolling statistic"
  )
  expect_error(
    augment_rolling(
      vehicles,
      value_col = "production",
      window = 1,
      .quiet = TRUE
    ),
    "at least 2"
  )
  expect_error(
    augment_rolling(
      vehicles,
      value_col = "production",
      align = "middle",
      .quiet = TRUE
    ),
    "align"
  )
})

## Messaging ------------------------------------------------------------------

test_that(".quiet controls informational output", {
  expect_silent(
    augment_rolling(
      vehicles,
      value_col = "production",
      window = 12,
      .quiet = TRUE
    )
  )
  expect_message(
    augment_rolling(
      vehicles,
      value_col = "production",
      window = 12,
      frequency = 12
    ),
    "12-period rolling sum"
  )
  expect_message(
    augment_rolling(
      retail_volume,
      group_cols = "name_series",
      window = 12,
      frequency = 12
    ),
    "for 9 groups"
  )
})

## Relationship to augment_trends() -------------------------------------------

test_that("rolling mean matches the right-aligned moving average trend", {
  rolled <- augment_rolling(
    vehicles,
    value_col = "production",
    stats = "mean",
    window = 12,
    .quiet = TRUE
  )
  trended <- augment_trends(
    vehicles,
    value_col = "production",
    methods = "ma",
    window = 12,
    align = "right",
    .quiet = TRUE
  )

  expect_equal(
    rolled$roll_mean_12[order(rolled$date)],
    trended$trend_ma[order(trended$date)]
  )
})

test_that("rolling statistics are kept out of the trend method registry", {
  # Guards the invariant that detrend_series() can never subtract an
  # aggregation from the series
  expect_length(intersect(.valid_rolling_stats(), .valid_methods()), 0)
  expect_error(
    augment_trends(
      vehicles,
      value_col = "production",
      methods = "chain",
      .quiet = TRUE
    )
  )
})

## Checks run once for the whole call -----------------------------------------

test_that("the chain scale guard fires on a grouped call", {
  data <- data.frame(
    date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 14), 2),
    # Percentage points, but declared as decimals
    value = c(rep(0.8, 14), rep(0.9, 14)),
    grp = rep(c("a", "b"), each = 14)
  )

  expect_warning(
    augment_rolling(
      data,
      group_cols = "grp",
      stats = "chain",
      window = 3,
      .quiet = TRUE
    ),
    "look like percentages"
  )
})

test_that("the chain scale guard warns once, not once per group", {
  data <- data.frame(
    date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 14), 3),
    value = rep(0.8, 42),
    grp = rep(c("a", "b", "c"), each = 14)
  )

  warnings <- character()
  withCallingHandlers(
    augment_rolling(
      data,
      group_cols = "grp",
      stats = "chain",
      window = 3,
      .quiet = TRUE
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1)
})

test_that("a mid-year start warns once on a grouped year-to-date call", {
  data <- data.frame(
    date = rep(seq(as.Date("2020-07-01"), by = "month", length.out = 12), 2),
    value = rep(1, 24),
    grp = rep(c("a", "b"), each = 12)
  )

  expect_warning(
    augment_rolling(
      data,
      group_cols = "grp",
      window = "ytd",
      .quiet = TRUE
    ),
    "first year is incomplete"
  )
})

## Groups shorter than the window ---------------------------------------------

test_that("groups shorter than the window are all named in the error", {
  base <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  data <- rbind(
    data.frame(date = base, value = 1:24, grp = "long"),
    data.frame(date = base[1:3], value = 1:3, grp = "tiny"),
    data.frame(date = base[1:5], value = 1:5, grp = "small")
  )

  expect_error(
    augment_rolling(data, group_cols = "grp", window = 12, .quiet = TRUE),
    "tiny"
  )
  expect_error(
    augment_rolling(data, group_cols = "grp", window = 12, .quiet = TRUE),
    "small"
  )
  # The group that is long enough is not blamed
  message <- tryCatch(
    augment_rolling(data, group_cols = "grp", window = 12, .quiet = TRUE),
    error = conditionMessage
  )
  expect_false(grepl("long", message))
})

test_that("a year-to-date window has no length requirement", {
  base <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  data <- rbind(
    data.frame(date = base, value = 1:24, grp = "long"),
    data.frame(date = base[1:3], value = 1:3, grp = "tiny")
  )

  expect_no_error(
    augment_rolling(data, group_cols = "grp", window = "ytd", .quiet = TRUE)
  )
})

## Even centred windows -------------------------------------------------------

test_that("a centred even mean matches augment_trends()", {
  rolled <- augment_rolling(
    vehicles,
    value_col = "production",
    stats = "mean",
    window = 12,
    align = "center",
    .quiet = TRUE
  )
  trended <- augment_trends(
    vehicles,
    value_col = "production",
    methods = "ma",
    window = 12,
    align = "center",
    .quiet = TRUE
  )

  expect_equal(rolled$roll_mean_12, trended$trend_ma)
})

## Irregular daily series -----------------------------------------------------

test_that("rolling aggregations on a daily series land on the input dates", {
  coffee <- coffee_arabica[, c("date", "usd_2022")]

  result <- augment_rolling(
    coffee,
    value_col = "usd_2022",
    stats = "sum",
    window = 22,
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(coffee))
  expect_equal(result$date, coffee$date)
  expect_equal(sum(is.na(result$roll_sum_22)), 21)
  expect_equal(result$roll_sum_22[100], sum(coffee$usd_2022[79:100]))
})

test_that("grouped daily rolling sums keep one row per input row", {
  coffee <- rbind(
    data.frame(crop = "arabica", coffee_arabica[, c("date", "usd_2022")]),
    data.frame(crop = "robusta", coffee_robusta[, c("date", "usd_2022")])
  )

  result <- augment_rolling(
    coffee,
    value_col = "usd_2022",
    group_cols = "crop",
    stats = "sum",
    window = 22,
    .quiet = TRUE
  )

  expect_equal(nrow(result), nrow(coffee))
  expect_equal(sort(table(result$crop)), sort(table(coffee$crop)))
})

test_that("year-to-date on a daily series resets on the calendar year", {
  coffee <- coffee_arabica[, c("date", "usd_2022")]

  result <- suppressWarnings(augment_rolling(
    coffee,
    value_col = "usd_2022",
    stats = "sum",
    window = "ytd",
    .quiet = TRUE
  ))

  # The first observation of a year accumulates nothing but itself
  year_start <- !duplicated(lubridate::year(result$date))
  expect_equal(result$roll_sum_ytd[year_start], result$usd_2022[year_start])

  # And the second adds exactly one more observation
  second <- which(year_start) + 1
  second <- second[second <= nrow(result)][-1]
  expect_equal(
    result$roll_sum_ytd[second],
    result$usd_2022[second] + result$usd_2022[second - 1]
  )
})

test_that("year-to-date is rejected for a daily ts carrying no dates", {
  daily <- stats::ts(as.numeric(1:500), start = c(2000, 1), frequency = 252)

  expect_error(
    roll_series(daily, "sum", window = "ytd", .quiet = TRUE),
    "not available"
  )
})
