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

