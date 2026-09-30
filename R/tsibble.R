# Tsibble input and output ----

#' @noRd
.tsibble_arguments <- function(
  data,
  date_col,
  group_cols,
  frequency,
  date_missing,
  group_missing
) {
  if (!requireNamespace("tsibble", quietly = TRUE)) {
    cli::cli_abort("The {.pkg tsibble} package is required for tsibble input.")
  }

  index_col <- tsibble::index_var(data)
  key_cols <- tsibble::key_vars(data)
  index <- data[[index_col]]
  ordered <- tsibble::as_tsibble(
    tibble::as_tibble(data),
    index = index_col,
    key = c(!!!rlang::syms(key_cols)),
    regular = tsibble::is_regular(data)
  )
  if (
    !identical(ordered[[index_col]], index) ||
      !all(vapply(
        key_cols,
        function(key) identical(ordered[[key]], data[[key]]),
        logical(1)
      ))
  ) {
    cli::cli_abort(
      "Tsibble rows must be ordered by key and index before computing trends."
    )
  }

  if (inherits(index, "yearmonth")) {
    index_frequency <- 12
  } else if (inherits(index, "yearquarter")) {
    index_frequency <- 4
  } else if (inherits(index, "Date")) {
    index_frequency <- NULL
  } else {
    cli::cli_abort(
      "Unsupported tsibble index class {.cls {class(index)[1]}}. Use a Date, yearmonth, or yearquarter index."
    )
  }

  if (!date_missing && !identical(date_col, index_col)) {
    cli::cli_abort(
      "{.arg date_col} must name the tsibble index {.val {index_col}}."
    )
  }
  if (date_missing) {
    date_col <- index_col
  }

  default_groups <- if (length(key_cols) == 0) NULL else key_cols
  same_groups <- is.character(group_cols) &&
    length(group_cols) == length(key_cols) &&
    setequal(group_cols, key_cols)
  if (
    !group_missing && !same_groups && !identical(group_cols, default_groups)
  ) {
    cli::cli_abort(
      "{.arg group_cols} must match the tsibble key {.val {key_cols}}."
    )
  }
  if (group_missing) {
    group_cols <- default_groups
  }
  if (length(group_cols) == 0) {
    group_cols <- NULL
  }
  if (is.null(frequency)) {
    frequency <- index_frequency
  }

  return(list(
    date_col = date_col,
    group_cols = group_cols,
    frequency = frequency
  ))
}

#' @noRd
.via_tsibble <- function(data, function_, ...) {
  index_col <- tsibble::index_var(data)
  key_cols <- tsibble::key_vars(data)
  plain <- tibble::as_tibble(data)
  if (!inherits(plain[[index_col]], "Date")) {
    plain[[index_col]] <- as.Date(plain[[index_col]])
  }

  result <- function_(plain, ...)
  if (
    nrow(result) != nrow(plain) ||
      !identical(result[[index_col]], plain[[index_col]]) ||
      !all(vapply(
        key_cols,
        function(key) identical(result[[key]], plain[[key]]),
        logical(1)
      ))
  ) {
    cli::cli_abort("The tsibble index or key changed during computation.")
  }

  new_cols <- setdiff(names(result), names(data))
  for (column in new_cols) {
    data[[column]] <- result[[column]]
  }
  return(data)
}
