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

#' Detect Peaks in 1D or 2D Data
#' This function detects peaks in either a numeric vector (1D) or a matrix (2D).
#' For 2D data, it can detect peaks row-wise or column-wise.
#' @param data A numeric vector (1D) or matrix (2D).
#' @param axis For 2D data, specify whether to detect peaks by "rows" or "columns".
#' @return For 1D data, returns a Peak object. For 2D data, returns a list of Peak objects for each row or column.
#' @export
detect_peaks <- function(data, axis = "rows") {
  # Handle 1D data (numeric vector)
  if (is.vector(data) && is.numeric(data)) {
    # Use pracma::findpeaks to detect peaks in 1D data
    peaks <- pracma::findpeaks(data, sortstr = TRUE)

    if (is.null(peaks)) {
      return(Peak(positions = numeric(0), heights = numeric(0)))
    }

    return(Peak(positions = peaks[, 2], heights = peaks[, 1]))
  }

  # Handle 2D data (matrix)
  if (is.matrix(data)) {
    peaks_list <- list()

    # Detect peaks row-wise or column-wise
    if (axis == "rows") {
      for (i in 1:nrow(data)) {
        row_peaks <- detect_peaks(data[i, ])  # Re-use the 1D peak detection logic
        peaks_list[[paste("row", i)]] <- row_peaks
      }
    } else if (axis == "columns") {
      for (i in 1:ncol(data)) {
        col_peaks <- detect_peaks(data[, i])  # Re-use the 1D peak detection logic
        peaks_list[[paste("column", i)]] <- col_peaks
      }
    } else {
      stop("Invalid axis. Choose either 'rows' or 'columns'.")
    }

    return(peaks_list)
  }

  stop("Input must be a numeric vector or a 2D matrix.")
}
