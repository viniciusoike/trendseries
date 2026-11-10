# invalid input raises informative errors

    Code
      aggregate_series(data, period = "week")
    Condition
      Error in `aggregate_series()`:
      ! `period` must be one of "month", "quarter", or "year", not "week".
    Code
      aggregate_series(data, summary = "median")
    Condition
      Error in `aggregate_series()`:
      ! `summary` must be one of "last", "first", "mean", "sum", "max", or "min", not "median".
      i Did you mean "mean"?
    Code
      aggregate_series(data, value_col = "price")
    Condition
      Error in `aggregate_series()`:
      ! Column not found in data: "price"
    Code
      aggregate_series(data, na_rm = NA)
    Condition
      Error in `aggregate_series()`:
      ! `na_rm` must be one non-missing logical value

---

    Code
      aggregate_series(duplicated)
    Condition
      Error in `aggregate_series()`:
      ! Dates must not be duplicated within a series or group.
      i If the data contains stacked series, identify them with `group_cols`.

---

    Code
      aggregate_series(missing_date)
    Condition
      Error in `aggregate_series()`:
      ! Column "date" must not contain missing dates

# a period no coarser than the data warns

    Code
      result <- aggregate_series(data, period = "month")
    Condition
      Warning:
      No month holds more than one observation, so nothing was aggregated.
      i Choose a `period` coarser than the frequency of the data.

# grouping columns cannot double as date or value columns

    Code
      aggregate_series(data, group_cols = "date")
    Condition
      Error in `aggregate_series()`:
      ! `group_cols` must not include `date_col` or `value_col`.
      x Also used as date or value: "date"
    Code
      aggregate_series(data, value_col = "value", group_cols = c("asset", "value"))
    Condition
      Error in `aggregate_series()`:
      ! `group_cols` must not include `date_col` or `value_col`.
      x Also used as date or value: "value"

