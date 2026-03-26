#' Add trend columns to data frame
#'
#' @description
#' Pipe-friendly function that adds trend columns to a tibble or data.frame.
#' Designed for exploratory analysis of monthly and quarterly economic time series.
#' Supports multiple trend extraction methods and handles grouped data.
#'
#' @param data A `data.frame`, `tibble`, or `data.table` containing the time series data.
#' @param date_col Name of the date column. Defaults to `"date"`.
#'   Must be of class `Date`.
#' @param value_col Name of the value column(s). Defaults to `"value"`.
#'   Must be `numeric`. A character vector of length > 1 is accepted; trends are
#'   extracted for each column and named `trend_{method}_{col}` (e.g.
#'   `trend_stl_consumption`).
#' @param group_cols Optional grouping variables for multiple
#'   time series. Can be a character vector of column names.
#' @param group_vars Deprecated. Use `group_cols` instead.
#' @param methods Character vector of trend methods.
#'   Options: `"hp"`, `"bk"`, `"cf"`, `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`,
#'   `"bn"`, `"ucm"`, `"hamilton"`, `"spencer"`, `"ewma"`, `"wma"`,
#'   `"triangular"`, `"kernel"`, `"kalman"`, `"median"`, `"gaussian"`.
#'   Default is `"stl"`.
#' @param frequency The frequency of the series.
#'   Supports 4 (quarterly) or 12 (monthly). Will be auto-detected if not specified.
#' @param suffix Optional suffix for trend column names.
#'   If NULL, uses method names.
#' @param window Unified window/period parameter for moving
#'   average methods (ma, wma, triangular, stl, ewma, median, gaussian). Must be positive.
#'   If NULL, uses frequency-appropriate defaults. For EWMA, specifies the window
#'   size when using TTR's optimized implementation. Cannot be used simultaneously
#'   with `smoothing` for EWMA method.
#'   For `ma` and `median` methods, a numeric vector is accepted (e.g., `c(3, 6, 12)`),
#'   which adds one column per window value named `trend_ma_3`, `trend_ma_6`, etc.
#'   Other methods ignore extra values (with a warning).
#' @param smoothing Unified smoothing parameter for smoothing
#'   methods (hp, loess, spline, ewma, kernel, kalman).
#'   For hp: use large values (1600+) or small values (0-1) that get converted.
#'   For EWMA: specifies the alpha parameter (0-1) for traditional exponential smoothing.
#'   Cannot be used simultaneously with `window` for EWMA method.
#'   For kernel: multiplier of optimal bandwidth (1.0 = optimal, <1 = less smooth, >1 = more smooth).
#'   For kalman: controls the ratio of measurement to process noise (higher = more smoothing).
#'   For others: typically 0-1 range.
#' @param band Unified band parameter for bandpass filters
#'   (bk, cf). Both values must be positive.
#'   Provide as `c(low, high)` where low/high are periods in quarters, e.g., `c(6, 32)`.
#' @param align Unified alignment parameter for moving average
#'   methods (ma, wma, triangular, gaussian). Valid values: `"center"` (default, uses
#'   surrounding values), `"right"` (causal, uses past values only), `"left"` (anti-causal,
#'   uses future values only). Note: triangular only supports `"center"` and `"right"`.
#'   If NULL, uses `"center"` as default.
#' @param params Optional list of method-specific parameters for fine control.
#' @param .quiet If `TRUE`, suppress informational messages.
#'
#' @return A tibble with original data plus trend columns named `trend_{method}` or
#'   `trend_{method}_{suffix}` if suffix is provided.
#'
#' @importFrom cli cli_abort cli_inform cli_warn
#' @importFrom tibble as_tibble
#' @importFrom stats is.ts setNames
#'
#' @details
#' This function is designed for monthly (frequency = 12) and quarterly (frequency = 4)
#' economic data. It uses economic-appropriate defaults for all trend extraction methods.
#'
#' For grouped data, the function applies trend extraction to each group separately,
#' maintaining the original data structure while adding trend columns.
#'
#' @examples
#' # Simple STL decomposition on quarterly GDP construction data
#' gdp_construction |> augment_trends(value_col = "index")
#'
#' # Multiple smoothing methods with unified parameter
#' gdp_construction |>
#'   augment_trends(
#'     value_col = "index",
#'     methods = c("hp", "loess", "ewma"),
#'     smoothing = 0.3
#'   )
#'
#' # Moving averages with unified window on monthly data
#' vehicles |>
#'   tail(60) |>
#'   augment_trends(
#'     value_col = "production",
#'     methods = c("ma", "wma", "triangular"),
#'     window = 8
#'   )
#'
#' # Economic indicators with different methods
#' ibcbr |>
#'   tail(48) |>
#'   augment_trends(
#'     value_col = "index",
#'     methods = c("median", "kalman", "kernel"),
#'     window = 9,
#'     smoothing = 0.15
#'   )
#'
#' # Moving average with right alignment (causal filter)
#' vehicles |>
#'   tail(60) |>
#'   augment_trends(
#'     value_col = "production",
#'     methods = "ma",
#'     window = 12,
#'     align = "right"
#'   )
#'
#' # Advanced: fine-tune specific methods
#' electric |>
#'   tail(72) |>
#'   augment_trends(
#'     value_col = "consumption",
#'     methods = "median",
#'     window = 7
#'   )
#'
#' # Multiple MA windows in a single call (adds trend_ma_3, trend_ma_6, trend_ma_12)
#' vehicles |>
#'   tail(60) |>
#'   augment_trends(
#'     value_col = "production",
#'     methods = "ma",
#'     window = c(3, 6, 12)
#'   )
#'
#' @export
augment_trends <- function(data,
                          date_col = "date",
                          value_col = "value",
                          group_cols = NULL,
                          group_vars = NULL,
                          methods = "stl",
                          frequency = NULL,
                          suffix = NULL,
                          window = NULL,
                          smoothing = NULL,
                          band = NULL,
                          align = NULL,
                          params = list(),
                          .quiet = FALSE) {

  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame, tibble, or data.table")
  }

  if (!date_col %in% names(data)) {
    cli::cli_abort("Column {.val {date_col}} not found in data")
  }

  missing_value_cols <- setdiff(value_col, names(data))
  if (length(missing_value_cols) > 0) {
    cli::cli_abort("Column{?s} not found in data: {.val {missing_value_cols}}")
  }

  if (!inherits(data[[date_col]], "Date")) {
    cli::cli_abort("Column {.val {date_col}} must be of class Date")
  }

  non_numeric <- value_col[!vapply(data[value_col], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    cli::cli_abort("Column{?s} must be numeric: {.val {non_numeric}}")
  }

  # Validate methods
  valid_methods <- c(
    "hp", "bk", "cf", "ma", "stl", "loess", "spline", "poly",
    "bn", "ucm", "hamilton", "spencer",
    "ewma", "wma", "triangular",
    "kernel", "kalman", "median", "gaussian"
  )
  invalid_methods <- setdiff(methods, valid_methods)
  if (length(invalid_methods) > 0) {
    cli::cli_abort(
      "Invalid methods: {.val {invalid_methods}}.
       Valid options: {.val {valid_methods}}"
    )
  }

  # Handle deprecated group_vars
  if (!is.null(group_vars)) {
    cli::cli_warn("{.arg group_vars} is deprecated. Use {.arg group_cols} instead.")
    if (is.null(group_cols)) group_cols <- group_vars
  }

  # Validate group_cols if provided
  if (!is.null(group_cols)) {
    if (!is.character(group_cols)) {
      cli::cli_abort("{.arg group_cols} must be a character vector")
    }
    missing_group_cols <- setdiff(group_cols, names(data))
    if (length(missing_group_cols) > 0) {
      cli::cli_abort(
        "Group variables not found in data: {.val {missing_group_cols}}.
         Available columns: {.val {names(data)}}"
      )
    }
  }

  # Validate unified parameters
  if (!is.null(window) && (!is.numeric(window) || any(window <= 0))) {
    cli::cli_abort(
      "{.arg window} must be a positive numeric value or a vector of positive numeric values.",
      "i" = "Got: {.val {window}}"
    )
  }

  if (!is.null(smoothing) && (!is.numeric(smoothing) || length(smoothing) != 1)) {
    cli::cli_abort("{.arg smoothing} must be a single numeric value")
  }

  if (!is.null(band) && (!is.numeric(band) || length(band) != 2 || any(band <= 0))) {
    cli::cli_abort("{.arg band} must be a numeric vector of length 2 with positive values")
  }

  if (!is.null(align)) {
    if (!is.character(align) || length(align) != 1) {
      cli::cli_abort("{.arg align} must be a single character value")
    }
    if (!align %in% c("left", "center", "right")) {
      cli::cli_abort(
        "{.arg align} must be one of 'left', 'center', or 'right', got {.val {align}}"
      )
    }
  }

  if (!is.list(params)) {
    cli::cli_abort("{.arg params} must be a list")
  }

  # Convert to tibble for consistent handling
  data <- tibble::as_tibble(data)

  # Handle multiple value columns: recurse once per column, using the column
  # name as a suffix so results are named trend_{method}_{col} (e.g. trend_stl_consumption)
  if (length(value_col) > 1) {
    result <- data
    for (vc in value_col) {
      vc_suffix <- if (is.null(suffix)) vc else paste0(vc, "_", suffix)
      result <- augment_trends(
        result,
        date_col  = date_col,
        value_col = vc,
        group_cols = group_cols,
        methods   = methods,
        frequency = frequency,
        suffix    = vc_suffix,
        window    = window,
        smoothing = smoothing,
        band      = band,
        align     = align,
        params    = params,
        .quiet    = .quiet
      )
    }
    return(result)
  }

  # Handle vector window: expand ma/median methods into one call per window value
  if (!is.null(window) && length(window) > 1) {
    window_methods <- intersect(methods, .WINDOW_VECTOR_METHODS)
    other_methods  <- setdiff(methods, .WINDOW_VECTOR_METHODS)

    if (length(window_methods) == 0) {
      cli::cli_warn(c(
        "Multiple {.arg window} values are only supported for {.val ma} and {.val median} methods.",
        "i" = "Using first value ({window[1]}) for method(s) {.val {methods}}."
      ))
      window <- window[1]
    } else {
      result <- data

      if (length(other_methods) > 0) {
        result <- augment_trends(
          result, date_col = date_col, value_col = value_col,
          group_cols = group_cols, methods = other_methods,
          frequency = frequency, suffix = suffix,
          window = NULL, smoothing = smoothing, band = band,
          align = align, params = params, .quiet = .quiet
        )
      }

      for (w in window) {
        w_suffix <- if (is.null(suffix)) as.character(w) else paste0(as.character(w), "_", suffix)
        result <- augment_trends(
          result, date_col = date_col, value_col = value_col,
          group_cols = group_cols, methods = window_methods,
          frequency = frequency, suffix = w_suffix,
          window = w, smoothing = smoothing, band = band,
          align = align, params = params, .quiet = .quiet
        )
      }

      return(result)
    }
  }

  # Handle grouped vs ungrouped data
  if (is.null(group_cols)) {
    result <- .augment_trends_single(
      data = data,
      date_col = date_col,
      value_col = value_col,
      methods = methods,
      frequency = frequency,
      suffix = suffix,
      window = window,
      smoothing = smoothing,
      band = band,
      align = align,
      params = params,
      .quiet = .quiet
    )
  } else {
    result <- .augment_trends_grouped(
      data = data,
      date_col = date_col,
      value_col = value_col,
      group_vars = group_cols,
      methods = methods,
      frequency = frequency,
      suffix = suffix,
      window = window,
      smoothing = smoothing,
      band = band,
      align = align,
      params = params,
      .quiet = .quiet
    )
  }

  return(result)
}

#' Internal function for single series trend augmentation
#' @noRd
.augment_trends_single <- function(data,
                                  date_col,
                                  value_col,
                                  methods,
                                  frequency,
                                  suffix,
                                  window,
                                  smoothing,
                                  band,
                                  align,
                                  params,
                                  .quiet) {

  # Auto-detect frequency if not provided
  if (is.null(frequency)) {
    frequency <- .detect_frequency(data[[date_col]], .quiet = .quiet)
  }

  # Validate frequency is reasonable
  if (!is.null(frequency) && (frequency < 1 || frequency > 365)) {
    cli::cli_abort(
      "Frequency must be between 1 (annual) and 365 (daily), got {frequency}"
    )
  }

  # Warn for frequency-sensitive methods with non-standard frequencies
  if (!frequency %in% c(1, 4, 12) && any(methods %in% c("hp", "bk", "cf", "hamilton"))) {
    freq_sensitive <- intersect(methods, c("hp", "bk", "cf", "hamilton"))
    if (!.quiet) {
      cli::cli_warn(
        "Methods {.val {freq_sensitive}} are optimized for standard economic frequencies.
         Using frequency = {frequency} with default parameters may produce suboptimal results.
         Consider specifying parameters explicitly via the {.arg params} argument."
      )
    }
  }

  # Check for STL with non-seasonal data
  if ("stl" %in% methods && frequency == 1) {
    if (!.quiet) {
      cli::cli_inform(
        "STL requires seasonal data (frequency > 1). Will use HP filter fallback for non-seasonal data."
      )
    }
  }

  # No need to set defaults here, extract_trends will handle them

  # Convert to time series for trend extraction
  ts_data <- .df_to_ts_internal(data, date_col, value_col, frequency)

  # Check minimum observations
  min_obs <- 3 * frequency
  if (length(ts_data) < min_obs) {
    cli::cli_warn(
      "Series has {length(ts_data)} observations.
       Minimum {min_obs} recommended for reliable trend extraction."
    )
  }

  # Extract trends using new extract_trends function
  trends <- extract_trends(
    ts_data = ts_data,
    methods = methods,
    window = window,
    smoothing = smoothing,
    band = band,
    align = align,
    params = params,
    .quiet = .quiet
  )

  # Convert trends back to data frame
  # Pass method information for proper naming when single method used
  if (length(methods) == 1 && stats::is.ts(trends)) {
    trends_list <- setNames(list(trends), methods[1])
    trends_df <- .trends_to_df(trends_list, date_col, suffix)
  } else {
    trends_df <- .trends_to_df(trends, date_col, suffix)
  }

  # Merge with original data, handling naming conflicts
  result <- .safe_merge(data, trends_df, date_col, frequency)

  return(result)
}

#' Internal function for grouped series trend augmentation
#' @noRd
.augment_trends_grouped <- function(data,
                                   date_col,
                                   value_col,
                                   group_vars,
                                   methods,
                                   frequency,
                                   suffix,
                                   window,
                                   smoothing,
                                   band,
                                   align,
                                   params,
                                   .quiet) {

  # Validate group variables
  missing_groups <- setdiff(group_vars, names(data))
  if (length(missing_groups) > 0) {
    cli::cli_abort("Group variables not found: {.val {missing_groups}}")
  }

  # Split data by groups
  data_split <- split(data, data[group_vars])
  group_names <- names(data_split)

  # Detect frequency once from the first group
  if (is.null(frequency)) {
    frequency <- .detect_frequency(data_split[[1]][[date_col]], .quiet = .quiet)
  }

  # Print a single consolidated summary instead of per-group messages
  if (!.quiet) {
    cli::cli_inform(c(
      "Computing {length(methods)} method(s) for {length(group_names)} group(s):",
      "i" = "Methods: {.val {methods}}",
      "i" = "Groups: {.val {group_names}}"
    ))
  }

  # Apply trend extraction to each group (suppress per-group messages)
  results <- lapply(data_split, function(group_data) {
    .augment_trends_single(
      data = group_data,
      date_col = date_col,
      value_col = value_col,
      methods = methods,
      frequency = frequency,
      suffix = suffix,
      window = window,
      smoothing = smoothing,
      band = band,
      align = align,
      params = params,
      .quiet = TRUE
    )
  })

  # Combine results
  result <- do.call(rbind, results)
  rownames(result) <- NULL

  return(result)
}