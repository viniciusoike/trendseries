# quiet calls retain and consolidate fallback warnings

    Code
      result <- augment_trends(panel, group_cols = "group", methods = "stl",
        frequency = 1, .quiet = TRUE)
    Condition
      Warning:
      STL not applicable for non-seasonal data. Using HP filter instead.
      i Affected groups: "a" and "b"

