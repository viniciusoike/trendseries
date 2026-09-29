# mixed vector windows use the first window for other methods

    Code
      mixed <- extract_trends(series, methods = c("ma", "wma"), window = c(3, 6),
      .quiet = TRUE)
    Condition
      Warning:
      Multiple `window` values are only supported for "ma", "median", and "henderson" methods.
      i Using first value (3) for method(s) "wma".

# mixed vector windows also reach the data-frame interface

    Code
      mixed <- augment_trends(data, methods = c("ma", "wma"), window = c(3, 6),
      .quiet = TRUE)
    Condition
      Warning:
      Multiple `window` values are only supported for "ma", "median", and "henderson" methods. i Using first value (3) for method(s) "wma".

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

