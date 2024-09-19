#' Detect Peaks in 2D Data
#' This function uses `pracma::findpeaks` to detect peaks in a numeric vector.
#' @param data A numeric vector representing the data.
#' @return A Peak object with positions and heights of detected peaks.
#' @export
detect_peaks <- function(data) {
  # Use a peak detection algorithm from pracma
  peaks <- pracma::findpeaks(data, sortstr = TRUE)

  # Handle case where no peaks are found
  if (is.null(peaks)) {
    return(Peak(positions = numeric(0), heights = numeric(0)))
  }

  # Extract positions and heights from the result
  Peak(positions = peaks[, 2], heights = peaks[, 1])
}

