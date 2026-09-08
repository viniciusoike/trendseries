# UCM fallbacks retain the series shape for each requested type

    Code
      ucm_level <- extract_trends(ts_data, methods = "ucm", params = list(ucm_type = "level"),
      .quiet = TRUE)
    Condition
      Warning:
      UCM estimation failed, using fallback smoothing: all parameters were fixed

---

    Code
      ucm_trend <- extract_trends(ts_data, methods = "ucm", params = list(ucm_type = "trend"),
      .quiet = TRUE)
    Condition
      Warning:
      UCM estimation failed, using fallback smoothing: all parameters were fixed

---

    Code
      ucm_bsm <- extract_trends(ts_data, methods = "ucm", params = list(ucm_type = "BSM"),
      .quiet = TRUE)
    Condition
      Warning:
      UCM estimation failed, using fallback smoothing: all parameters were fixed

# enhanced parameters work with augment_trends

    Code
      result_ucm <- augment_trends(df_data, date_col = "date", value_col = "value",
        methods = "ucm", params = list(ucm_type = "trend"), .quiet = TRUE)
    Condition
      Warning:
      UCM estimation failed, using fallback smoothing: all parameters were fixed

# mixed vector windows use the first window for other methods

    Code
      mixed <- extract_trends(series, methods = c("ma", "wma"), window = c(3, 6),
      .quiet = TRUE)
    Condition
      Warning:
      Multiple `window` values are only supported for "ma", "median", and "henderson" methods.
      i Using first value (3) for method(s) "wma".

# invalid Kalman ratios and variances are rejected

    Code
      extract_trends(series, methods = "kalman", smoothing = NA_real_, .quiet = TRUE)
    Condition
      Error in `.kalman_smooth()`:
      ! Kalman `smoothing` must be one finite, positive noise ratio

---

    Code
      extract_trends(series, methods = "kalman", smoothing = 0, .quiet = TRUE)
    Condition
      Error in `.kalman_smooth()`:
      ! Kalman `smoothing` must be one finite, positive noise ratio

---

    Code
      extract_trends(series, methods = "kalman", params = list(kalman_process_noise = -
        1), .quiet = TRUE)
    Condition
      Error in `.kalman_smooth()`:
      ! Kalman "process_noise" must be one finite, non-negative variance

