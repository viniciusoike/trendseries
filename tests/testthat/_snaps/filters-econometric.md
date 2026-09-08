# UCM fallback returns a dated series

    Code
      ucm_trend <- extract_trends(ts_data, methods = "ucm", .quiet = TRUE)
    Condition
      Warning:
      UCM estimation failed, using fallback smoothing: all parameters were fixed

