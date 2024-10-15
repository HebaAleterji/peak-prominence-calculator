#' Calculate Prominence for Detected Peaks in 1D or 2D Data
#' The prominence is the vertical distance between the peak height and the higher minimum
#' between the peak's left and right neighbors.
#' @param peaks A PeakData object (1D) or a list of PeakData objects (2D).
#' @param data A numeric vector (1D) or matrix (2D) representing the original data.
#' @return A Prominence object (for 1D) or a list of Prominence objects (for 2D).
#' @export
calculate_prominence <- function(peaks, data) {
  if (is.vector(data) && is.numeric(data)) {
    # Handle 1D data
    if (length(peaks$positions) == 0) {
      return(as.prominence(positions = numeric(0), prominences = numeric(0)))
    }

    n_peaks <- length(peaks$positions)
    prominences <- numeric(n_peaks)

    for (i in seq_along(peaks$positions)) {
      peak_pos <- peaks$positions[i]
      peak_height <- peaks$heights[i]

      # Find the local minimum between this peak and the previous/next peak
      if (i == 1) {
        # If it's the first peak, find the minimum from the start to this peak
        left_min <- min(data[1:peak_pos])
      } else {
        left_min <- min(data[peaks$positions[i - 1]:peak_pos])
      }

      if (i == n_peaks) {
        # If it's the last peak, find the minimum from this peak to the end of the data
        right_min <- min(data[peak_pos:length(data)])
      } else {
        right_min <- min(data[peak_pos:peaks$positions[i + 1]])
      }

      # Prominence is the peak height minus the maximum of the left and right minima
      valley_min <- max(left_min, right_min)
      prominences[i] <- peak_height - valley_min
    }

    return(as.prominence(positions = peaks$positions, prominences = prominences))

  } else if (is.matrix(data)) {
    # Handle 2D data row-wise
    calculate_single_prominence <- function(i) {
      return(calculate_prominence(peaks[[i]], data[i, ]))
    }

    # Apply prominence calculation row-wise for 2D matrix
    result <- lapply(1:nrow(data), calculate_single_prominence)
    return(result)
  } else {
    stop("Data must be either a numeric vector (1D) or a numeric matrix (2D).")
  }
}
