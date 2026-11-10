# grouped bases use each series calendar

    Code
      index_series(rbind(monthly, quarterly[1, ]), group_cols = "group", base_period = 2020,
      .quiet = TRUE)
    Condition
      Error in `FUN()`:
      ! Need at least two dated observations to detect frequency for group group = c.

# a grouped tibble is indexed like a plain data frame

    Code
      index_series(data, value_col = "price")
    Condition
      Error in `index_series()`:
      ! Column not found in data: "price"

