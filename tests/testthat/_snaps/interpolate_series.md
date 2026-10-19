# inconsistent dates are normalized to period starts

    Code
      result <- interpolate_series(data, frequency = 12, method = "linear")
    Condition
      Warning:
      Inconsistent date conventions were normalized to period starts.

# Kalman non-convergence warns and keeps the Kalman estimates

    Code
      result <- interpolate_series(data, frequency = 12, method = method)
    Condition
      Warning:
      Kalman model did not converge for column "value"; estimates may be unreliable.

---

    Code
      result <- interpolate_series(data, frequency = 12, method = method)
    Condition
      Warning:
      Kalman model did not converge for column "value"; estimates may be unreliable.

# auto falls back to linear when Kalman interpolation fails

    Code
      result <- interpolate_series(data, frequency = 12, method = "auto")
    Condition
      Warning:
      Kalman interpolation failed for column "value"; used linear interpolation.
      i forced Kalman failure

---

    Code
      interpolate_series(data, frequency = 12, method = "kalman")
    Condition
      Error in `.interpolate_vector()`:
      ! Kalman interpolation failed for column "value".
      x forced Kalman failure

# the automatic maximum gap is enforced

    Code
      interpolate_series(data, frequency = 12, method = "linear")
    Condition
      Error in `.interpolate_vector()`:
      ! Missing gap is too long to interpolate in column "value".
      x Longest gap: 8 periods; maximum allowed: 7.

---

    Code
      result <- interpolate_series(data, frequency = 12, method = "linear", max_gap = Inf)
    Condition
      Warning:
      Interpolating 80% of the observed span in column "value".

# duplicate periods and insufficient observations are rejected

    Code
      interpolate_series(duplicates, frequency = 12, method = "linear")
    Condition
      Error in `.complete_interpolation_grid()`:
      ! Found 1 duplicated period in the data.
      x Duplicated: "2020-01-01"

---

    Code
      interpolate_series(sparse, frequency = 12, method = "linear")
    Condition
      Error in `.interpolate_vector()`:
      ! Column "value" needs at least two observed values.

# imputation flag conflicts are rejected

    Code
      interpolate_series(data, frequency = 12, method = "linear", imputed_col = TRUE)
    Condition
      Error in `.validate_interpolation_input()`:
      ! Imputation column name already present: "imputed_value".

