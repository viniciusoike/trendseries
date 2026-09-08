# quiet UCM calls report estimator fallback

    Code
      result <- augment_trends(gdp_construction, value_col = "index", methods = "ucm",
        .quiet = TRUE)
    Condition
      Warning:
      UCM estimation failed, using fallback smoothing: fit failed

# quiet calls retain and consolidate fallback warnings

    Code
      result <- augment_trends(panel, group_cols = "group", methods = "stl",
        frequency = 1, .quiet = TRUE)
    Condition
      Warning:
      STL not applicable for non-seasonal data. Using HP filter instead.
      i Affected groups: "a" and "b"

