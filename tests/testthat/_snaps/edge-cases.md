# leading and trailing missing values are trimmed, not rejected

    Code
      result <- extract_trends(padded, methods = method, .quiet = TRUE)
    Condition
      Warning:
      UCM estimation failed, using fallback smoothing: all parameters were fixed

# a complete series is untouched by the trimming path

    Code
      result <- extract_trends(series, methods = method, .quiet = TRUE)
    Condition
      Warning:
      UCM estimation failed, using fallback smoothing: all parameters were fixed

# daily and weekly trends reject explicitly missing interior values

    Code
      augment_trends(data, methods = "ma", window = 3, frequency = frequency, .quiet = TRUE)
    Condition
      Error in `.check_regular_grid()`:
      ! Series has 1 interior observation with missing values.
      x Missing: "2020-01-05"
      i Impute interior missing values before extracting trends or decomposing the series.

---

    Code
      decompose_series(data, methods = "regression", frequency = frequency, .quiet = TRUE)
    Condition
      Error in `.check_regular_grid()`:
      ! Series has 1 interior observation with missing values.
      x Missing: "2020-01-05"
      i Impute interior missing values before extracting trends or decomposing the series.

---

    Code
      augment_trends(data, methods = "ma", window = 3, frequency = frequency, .quiet = TRUE)
    Condition
      Error in `.check_regular_grid()`:
      ! Series has 1 interior observation with missing values.
      x Missing: "2020-01-29"
      i Impute interior missing values before extracting trends or decomposing the series.

---

    Code
      decompose_series(data, methods = "regression", frequency = frequency, .quiet = TRUE)
    Condition
      Error in `.check_regular_grid()`:
      ! Series has 1 interior observation with missing values.
      x Missing: "2020-01-29"
      i Impute interior missing values before extracting trends or decomposing the series.

# date names can overlap generated names without losing columns

    Code
      result <- compute(renamed, generated)
    Condition
      Warning:
      Column "trend_ma" already exists. Renamed new column to "trend_ma_2"

---

    Code
      result <- compute(renamed, generated)
    Condition
      Warning:
      Column "roll_sum_3" already exists. Renamed new column to "roll_sum_3_2"

---

    Code
      result <- compute(renamed, generated)
    Condition
      Warning:
      Column "trend_regression" already exists. Renamed new column to "trend_regression_2"

