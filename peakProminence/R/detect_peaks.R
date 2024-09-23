#' #' Detect Peaks in 2D Data
#' #' This function uses `pracma::findpeaks` to detect peaks in a numeric vector.
#' #' @param data A numeric vector representing the data.
#' #' @return A Peak object with positions and heights of detected peaks.
#' #' @export
#' detect_peaks <- function(data) {
#'   # Use a peak detection algorithm from pracma
#'   peaks <- pracma::findpeaks(data, sortstr = TRUE)
#'
#'   # Handle case where no peaks are found
#'   if (is.null(peaks)) {
#'     return(Peak(positions = numeric(0), heights = numeric(0)))
#'   }
#'
#'   # Extract positions and heights from the result
#'   Peak(positions = peaks[, 2], heights = peaks[, 1])
#' }
#'

#' Detect Peaks in 1D and 2D Data
#' This function detects peaks in either 1D or 2D data. For 1D, it uses
#' `pracma::findpeaks` to detect local maxima. For 2D data, it detects
#' peaks along each column.
#' @param data A numeric vector (1D) or matrix (2D) representing the data.
#' @return A list of Peak objects, or a single Peak object for 1D data.
#' @export
detect_peaks <- function(data) {
  # Check if the input is a matrix (2D data)
  if (is.matrix(data)) {
    peak_list <- list()
    for (i in 1:ncol(data)) {
      column_data <- data[, i]
      peaks <- pracma::findpeaks(column_data, sortstr = TRUE)

      if (is.null(peaks)) {
        peak_list[[i]] <- Peak(positions = numeric(0), heights = numeric(0))
      } else {
        peak_list[[i]] <- Peak(positions = peaks[, 2], heights = peaks[, 1])
      }
    }
    return(peak_list)

    # If the input is not a matrix, treat it as 1D data
  } else if (is.vector(data)) {
    peaks <- pracma::findpeaks(data, sortstr = TRUE)

    if (is.null(peaks)) {
      return(Peak(positions = numeric(0), heights = numeric(0)))
    }

    return(Peak(positions = peaks[, 2], heights = peaks[, 1]))
  } else {
    stop("Input data must be either a numeric vector (1D) or matrix (2D).")
  }
}

