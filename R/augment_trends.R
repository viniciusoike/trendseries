#' Add trend columns to data frame
#'
#' @description
#' Pipe-friendly function that adds trend columns to a tibble or data.frame.
#' Designed for exploratory analysis of monthly and quarterly economic time series.
#' Supports multiple trend extraction methods and handles grouped data.
#'
#' @param data A `data.frame`, `tibble`, or `data.table`
#' @param date_col Name of the date column. Defaults to `"date"`. Must be of class `Date`.
#' @param value_col Name of the value column. Defaults to `"value"`. Must be `numeric`.
#' @param group_vars Optional grouping variables for multiple time series.
#'   Can be a character vector of column names.
#' @param methods Character vector of trend methods. Options: `"hp"`, `"bk"`, `"cf"`,
#'   `"ma"`, `"stl"`, `"loess"`, `"spline"`, `"poly"`, `"bn"`, `"ucm"`, `"hamilton"`,
#'   `"exp_simple"`, `"exp_double"`, `"ewma"`, `"alma"`, `"dema"`, `"hma"`, `"sg"`,
#'   `"kernel"`, `"butter"`, `"kalman"`, `"wavelet"`. Default is `"hp"`.
#' @param frequency The frequency of the series. Supports 4 (quarterly) or 12 (monthly).
#'   Will be auto-detected if not specified.
#' @param suffix Optional suffix for trend column names. If NULL, uses method names.
#' @param window Unified window/period parameter for moving average methods.
#' @param smoothing Unified smoothing parameter for smoothing methods.
#' @param band Unified band parameter for bandpass filters as c(low, high).
#' @param params Optional list of method-specific parameters for fine control.
#' @param .quiet If TRUE, suppress informational messages.
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
#' # Simple HP filter on quarterly GDP construction data
#' gdp_construction |> augment_trends(value_col = "gdp_construction")
#'
#' # Multiple smoothing methods with unified parameter
#' gdp_construction |>
#'   augment_trends(
#'     value_col = "gdp_construction",
#'     methods = c("hp", "loess", "ewma"),
#'     smoothing = 0.3
#'   )
#'
#' # Moving averages with unified window on monthly data
#' vehicles |>
#'   tail(60) |>
#'   augment_trends(
#'     value_col = "vehicles",
#'     methods = c("ma", "dema", "hma"),
#'     window = 8
#'   )
#'
#' # Economic indicators with different methods
#' ibcbr |>
#'   tail(48) |>
#'   augment_trends(
#'     value_col = "ibcbr",
#'     methods = c("sg", "kalman", "kernel"),
#'     window = 9,
#'     smoothing = 0.15
#'   )
#'
#' # Advanced: fine-tune specific methods
#' electric |>
#'   tail(72) |>
#'   augment_trends(
#'     value_col = "electric",
#'     methods = c("sg", "wavelet"),
#'     window = 7,
#'     params = list(sg_poly_order = 3, wavelet_type = "db4")
#'   )
#'
#' @export
augment_trends <- function(data,
                          date_col = "date",
                          value_col = "value",
                          group_vars = NULL,
                          methods = "hp",
                          frequency = NULL,
                          suffix = NULL,
                          window = NULL,
                          smoothing = NULL,
                          band = NULL,
                          params = list(),
                          .quiet = FALSE) {

  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame, tibble, or data.table")
  }

  if (!date_col %in% names(data)) {
    cli::cli_abort("Column {.val {date_col}} not found in data")
  }

  if (!value_col %in% names(data)) {
    cli::cli_abort("Column {.val {value_col}} not found in data")
  }

  if (!inherits(data[[date_col]], "Date")) {
    cli::cli_abort("Column {.val {date_col}} must be of class Date")
  }

  if (!is.numeric(data[[value_col]])) {
    cli::cli_abort("Column {.val {value_col}} must be numeric")
  }

  # Validate methods
  valid_methods <- c("hp", "bk", "cf", "ma", "stl", "loess", "spline", "poly",
                     "bn", "ucm", "hamilton", "exp_simple", "exp_double",
                     "ewma", "alma", "dema", "hma", "sg", "kernel", "butter",
                     "kalman", "wavelet")
  invalid_methods <- setdiff(methods, valid_methods)
  if (length(invalid_methods) > 0) {
    cli::cli_abort(
      "Invalid methods: {.val {invalid_methods}}.
       Valid options: {.val {valid_methods}}"
    )
  }

  # Convert to tibble for consistent handling
  data <- tibble::as_tibble(data)

  # Handle grouped vs ungrouped data
  if (is.null(group_vars)) {
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
      params = params,
      .quiet = .quiet
    )
  } else {
    result <- .augment_trends_grouped(
      data = data,
      date_col = date_col,
      value_col = value_col,
      group_vars = group_vars,
      methods = methods,
      frequency = frequency,
      suffix = suffix,
      window = window,
      smoothing = smoothing,
      band = band,
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
                                  params,
                                  .quiet) {

  # Auto-detect frequency if not provided
  if (is.null(frequency)) {
    frequency <- .detect_frequency(data[[date_col]], .quiet = .quiet)
  }

  # Validate frequency for economic data focus
  if (!frequency %in% c(4, 12)) {
    cli::cli_abort(
      "Only monthly (12) and quarterly (4) frequencies are supported.
       Detected frequency: {frequency}"
    )
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
  result <- .safe_merge(data, trends_df, date_col)

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
                                   params,
                                   .quiet) {

  # Validate group variables
  missing_groups <- setdiff(group_vars, names(data))
  if (length(missing_groups) > 0) {
    cli::cli_abort("Group variables not found: {.val {missing_groups}}")
  }

  # Split data by groups
  data_split <- split(data, data[group_vars])

  # Apply trend extraction to each group
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
      params = params,
      .quiet = .quiet
    )
  })

  # Combine results
  result <- do.call(rbind, results)
  rownames(result) <- NULL

  return(result)
}