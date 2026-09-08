test_that("Adding series work", {
  expect_true(is.data.frame(ts_to_df(AirPassengers)))
  expect_true(tibble::is_tibble(ts_to_df(AirPassengers)))
  expect_error(ts_to_df(1:10))
})

## Period positioning ---------------------------------------------------------

# `ts()` reads `start = c(year, period)` in units of `frequency`, and positions
# every later observation by counting from there. Each of these once produced a
# plausible-looking series carrying the wrong dates.

make_series <- function(start, by, n) {
  return(data.frame(
    date = seq(as.Date(start), by = by, length.out = n),
    value = seq_len(n)
  ))
}

test_that("the start period is counted in units of the frequency", {
  # Taking the calendar month regardless of frequency put this at Q4
  quarterly <- make_series("2010-04-01", "quarter", 8)
  expect_equal(start(df_to_ts(quarterly, frequency = 4)), c(2010, 2))

  quarterly_q4 <- make_series("2010-10-01", "quarter", 8)
  expect_equal(start(df_to_ts(quarterly_q4, frequency = 4)), c(2010, 4))

  # An annual series dated March started at year + 2
  annual <- make_series("2010-03-01", "year", 5)
  expect_equal(start(df_to_ts(annual, frequency = 1)), c(2010, 1))

  semiannual <- make_series("2010-07-01", "6 months", 6)
  expect_equal(start(df_to_ts(semiannual, frequency = 2)), c(2010, 2))

  monthly <- make_series("2010-06-01", "month", 24)
  expect_equal(start(df_to_ts(monthly, frequency = 12)), c(2010, 6))
})

test_that("weekly and daily starts are placed within the year", {
  # No exact calendar period, so the position is proportional rather than 1
  weekly <- make_series("2010-07-05", "week", 60)
  expect_equal(start(df_to_ts(weekly, frequency = 52)), c(2010, 27))
})

test_that("rows are sorted before conversion", {
  data <- make_series("2020-01-01", "month", 12)

  expect_equal(
    as.numeric(df_to_ts(data[12:1, ], frequency = 12)),
    as.numeric(df_to_ts(data, frequency = 12))
  )
})

test_that("df_to_ts() rejects an incomplete period grid", {
  data <- make_series("2020-01-01", "month", 12)

  # A gap shifts every later observation one slot earlier and drops December
  expect_error(df_to_ts(data[-6, ], frequency = 12), "missing period")
  expect_error(
    df_to_ts(rbind(data, data[3, ]), frequency = 12),
    "duplicated period"
  )
})

test_that("rows with no date are dropped", {
  data <- make_series("2020-01-01", "month", 12)
  data$date[3] <- NA

  # The row cannot be positioned, and dropping it opens a gap
  expect_warning(
    expect_error(df_to_ts(data, frequency = 12), "missing period"),
    "missing"
  )

  expect_warning(
    expect_error(
      df_to_ts(transform(data, date = as.Date(NA)), frequency = 12),
      "No dated observations"
    ),
    "Dropped"
  )
})

test_that("missing values are kept in place", {
  data <- make_series("2020-01-01", "month", 12)
  data$value[6] <- NA

  result <- suppressWarnings(df_to_ts(data, frequency = 12))

  # Keeping the row is what leaves the series correctly dated
  expect_equal(length(result), 12)
  expect_equal(end(result), c(2020, 12))
  expect_true(is.na(result[6]))
  expect_warning(df_to_ts(data, frequency = 12), "Missing values")
})

test_that("bundled datasets convert without loss", {
  result <- df_to_ts(ibcbr, value_col = "index", frequency = "M")

  expect_equal(length(result), nrow(ibcbr))
  expect_equal(start(result), c(2003, 1))
  expect_equal(as.numeric(result), ibcbr$index)
})
