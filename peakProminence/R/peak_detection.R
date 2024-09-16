#' Custom Detect Peaks in 1D or 2D Data with Smoothing
#'
#' This function detects peaks by identifying local maxima in a numeric vector or matrix.
#' For 2D data (matrix), it applies peak detection row-wise or column-wise, and smooths the data before detection.
#'
#' @param data A numeric vector (1D) or matrix (2D) containing the data in which to detect peaks.
#' @param min_peak_height A numeric value for the minimum peak height (default is 0).
#' @param smoothing A logical value indicating whether to apply smoothing (default is TRUE).
#' @param apply_across Either "row" or "column" to specify if peaks should be detected row-wise or column-wise for 2D data (default is "row").
#' @return A list of data frames, each containing the positions and heights of detected peaks for each row or column in 2D data. For 1D data, returns a single data frame.
#' @export
detect_peaks <- function(data, min_peak_height = 0, smoothing = TRUE, apply_across = "row") {

  # Helper function for simple moving average smoothing
  smooth_data <- function(vec) {
    n <- 3 # 3-point moving average
    filter(vec, rep(1 / n, n), sides = 2)
  }

  # Helper function to find peaks in 1D data
  find_peaks_custom <- function(vec, min_peak_height) {
    peak_positions <- c()
    peak_heights <- c()

    # Apply smoothing if needed
    if (smoothing) {
      vec <- smooth_data(vec)
    }

    # Loop through the vector, starting from the second element to the second-to-last
    for (i in 2:(length(vec) - 1)) {
      # Check if vec[i] is greater than its neighbors and meets the height threshold
      if (vec[i] > vec[i - 1] && vec[i] > vec[i + 1] && vec[i] >= min_peak_height) {
        peak_positions <- c(peak_positions, i)
        peak_heights <- c(peak_heights, vec[i])
      }
    }

    return(data.frame(Position = peak_positions, Height = peak_heights))
  }

  # Check if data is a numeric vector (1D) or matrix (2D)
  if (is.vector(data) && is.numeric(data)) {
    # For 1D data, apply custom peak detection
    return(find_peaks_custom(data, min_peak_height))

  } else if (is.matrix(data) && is.numeric(data)) {
    # For 2D data (matrix), apply peak detection row-wise or column-wise
    result_list <- list()

    if (apply_across == "row") {
      for (i in 1:nrow(data)) {
        result_list[[i]] <- find_peaks_custom(data[i, ], min_peak_height)
      }
    } else if (apply_across == "column") {
      for (i in 1:ncol(data)) {
        result_list[[i]] <- find_peaks_custom(data[, i], min_peak_height)
      }
    } else {
      stop("Invalid option for 'apply_across'. Use 'row' or 'column'.")
    }

    return(result_list)
  } else {
    stop("Input data must be either a numeric vector or matrix.")
  }
}
