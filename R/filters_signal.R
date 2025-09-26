#' Signal Processing Methods
#'
#' @description Internal functions for signal processing-based trend extraction
#' methods including Savitzky-Golay filtering, kernel smoothing, Butterworth
#' filtering, and Kalman smoothing.
#'
#' @name signal-filters
#' @keywords internal

#' Extract Savitzky-Golay trend
#' @noRd
.extract_sg_trend <- function(ts_data, window, poly_order, .quiet) {
  if (!.quiet) {
    cli::cli_inform(
      "Computing Savitzky-Golay filter with window = {window}, polynomial order = {poly_order}"
    )
  }

  return(.savitzky_golay(ts_data, window, poly_order))
}

#' Savitzky-Golay filter implementation
#' @noRd
.savitzky_golay <- function(ts_data, window = 7, poly_order = 2) {
  # Use signal package's optimized implementation
  y <- as.numeric(ts_data)

  # Use signal::sgolayfilt
  filtered <- signal::sgolayfilt(y, p = poly_order, n = window)

  trend_ts <- stats::ts(
    filtered,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}


#' Extract kernel smoother trend
#' @noRd
.extract_kernel_trend <- function(ts_data, bandwidth, kernel_type, .quiet) {
  if (!.quiet) {
    bandwidth_msg <- if (is.null(bandwidth)) "auto" else "{bandwidth}"
    cli::cli_inform(
      "Computing kernel smoother with bandwidth = {bandwidth_msg}, kernel = {kernel_type}"
    )
  }

  return(.kernel_smooth(ts_data, bandwidth, kernel_type))
}

#' Kernel smoothing implementation
#' @noRd
.kernel_smooth <- function(ts_data, bandwidth = NULL, kernel = "normal") {
  time_index <- as.numeric(stats::time(ts_data))
  values <- as.numeric(ts_data)

  # Auto-select bandwidth using rule-of-thumb if not provided
  if (is.null(bandwidth)) {
    bandwidth <- stats::bw.nrd0(time_index)
  }

  # Use stats::ksmooth for kernel regression
  # This is equivalent to Nadaraya-Watson estimator
  smooth_result <- stats::ksmooth(
    x = time_index,
    y = values,
    kernel = kernel,
    bandwidth = bandwidth,
    x.points = time_index
  )

  trend_ts <- stats::ts(
    smooth_result$y,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

#' Extract Butterworth trend
#' @noRd
.extract_butter_trend <- function(ts_data, cutoff, order, .quiet) {
  if (!.quiet) {
    cli::cli_inform(
      "Computing Butterworth filter with cutoff = {cutoff}, order = {order}"
    )
  }

  return(.butterworth_filter(ts_data, cutoff, order))
}

#' Butterworth low-pass filter
#' @noRd
.butterworth_filter <- function(ts_data, cutoff = 0.1, order = 2) {
  # Use signal package's optimized Butterworth filter
  y <- as.numeric(ts_data)

  # Design Butterworth filter
  # Cutoff frequency should be normalized (0-1, where 1 is Nyquist)
  cutoff_norm <- min(cutoff, 0.49)  # Ensure below Nyquist

  # Design and apply filter
  bf <- signal::butter(order, cutoff_norm, type = "low")
  filtered <- signal::filtfilt(bf, y)

  trend_ts <- stats::ts(
    filtered,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}


#' Extract Kalman smoother trend
#' @noRd
.extract_kalman_trend <- function(
  ts_data,
  measurement_noise,
  process_noise,
  .quiet
) {
  if (!.quiet) {
    noise_msg <- if (is.null(measurement_noise)) {
      "auto"
    } else {
      "{measurement_noise}"
    }
    cli::cli_inform(
      "Computing Kalman smoother with measurement noise = {noise_msg}"
    )
  }

  return(.kalman_smooth(ts_data, measurement_noise, process_noise))
}

#' Kalman smoothing implementation
#' @noRd
.kalman_smooth <- function(
  ts_data,
  measurement_noise = NULL,
  process_noise = NULL
) {
  # Use dlm package's optimized Kalman filtering
  y <- as.numeric(ts_data)

  if (is.null(measurement_noise) || is.null(process_noise)) {
    # Auto-estimate noise parameters
    y_var <- stats::var(y, na.rm = TRUE)
    measurement_noise <- y_var * 0.1  # 10% of signal variance
    process_noise <- y_var * 0.01     # 1% of signal variance
  }

  # Build local level model (random walk + noise)
  mod <- dlm::dlmModPoly(order = 1, dV = measurement_noise, dW = process_noise)

  # Apply Kalman filter and smoother
  filtered <- dlm::dlmFilter(y, mod)
  smoothed <- dlm::dlmSmooth(filtered)

  # Extract smoothed states (trend component)
  if (is.matrix(smoothed$s)) {
    trend_values <- smoothed$s[-1, 1]  # Remove initial state, take level component
  } else {
    trend_values <- smoothed$s[-1]  # Vector case
  }

  trend_ts <- stats::ts(
    trend_values,
    start = stats::start(ts_data),
    frequency = stats::frequency(ts_data)
  )
  return(trend_ts)
}

