#' Detect Peaks in 1D or 2D Data with Adaptive Noise Threshold
#' This function detects peaks in a numeric vector (1D) or numeric matrix (2D).
#' If no noise_threshold is provided, it is set adaptively based on the data.
#' @param data A numeric vector (1D) or matrix (2D) representing the data.
#' @param noise_threshold A numeric value specifying the minimum height a peak
#'        must have to be considered significant. If NULL, it will be calculated
#'        as a fraction of the standard deviation of the data.
#' @param min_distance An integer specifying the minimum distance between peaks.
#'        This helps avoid detecting multiple close peaks in noisy regions. Defaults to 1.
#' @return A PeakData object with positions and heights of detected peaks for 1D data,
#'         or a list of PeakData objects for each row of 2D data.
#' @export
detect_peaks <- function(data, noise_threshold = NULL, min_distance = 1) {
  # Validate input early: Ensure data is numeric (either vector or matrix)
  if (!(is.numeric(data) || (is.matrix(data) && is.numeric(data[1, ])))) {
    stop("Data must be either a numeric vector (1D) or a numeric matrix (2D).")
  }

  # Handle empty input with a message
  if (length(data) == 0) {
    message("Warning: No data provided. Returning empty peak results.")
    return(as.peak(positions = numeric(0), heights = numeric(0)))
  }

  # Automatically set noise_threshold if not provided
  if (is.null(noise_threshold)) {
    noise_threshold <- 0.1 * sd(data, na.rm = TRUE)  # Handle numeric data safely
  }

  if (is.vector(data) && is.numeric(data)) {
    # Handle 1D data
    peaks <- pracma::findpeaks(data, minpeakheight = noise_threshold, minpeakdistance = min_distance, sortstr = TRUE)

    if (is.null(peaks)) {
      return(as.peak(positions = numeric(0), heights = numeric(0)))
    }

    positions <- peaks[, 2]
    heights <- peaks[, 1]
    return(as.peak(positions = positions, heights = heights))

  } else if (is.matrix(data)) {
    # Handle 2D data row-wise
    detect_single_peaks <- function(row_data) {
      peaks <- pracma::findpeaks(row_data, minpeakheight = noise_threshold, minpeakdistance = min_distance, sortstr = TRUE)
      if (is.null(peaks)) {
        return(as.peak(positions = numeric(0), heights = numeric(0)))
      }
      positions <- peaks[, 2]
      heights <- peaks[, 1]
      return(as.peak(positions = positions, heights = heights))
    }

    result <- apply(data, 1, detect_single_peaks)
    return(result)

  } else {
    stop("Data must be either a numeric vector (1D) or a numeric matrix (2D).")
  }
}
