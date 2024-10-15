#' Calculate Prominence for Detected Peaks
#'
#' This function calculates the prominence of each peak as the height difference
#' between the peak and the lowest point in the surrounding region that separates
#' it from neighboring higher peaks.
#'
#' @param peaks A Peak object containing peak positions and heights.
#' @param data A numeric vector representing the original data.
#' @return A Prominence object with calculated prominence values for each peak.
#' @export
calculate_prominence <- function(peaks, data) {
  if (length(peaks$positions) == 0) {
    return(Prominence(positions = numeric(0), prominences = numeric(0)))
  }

  n_peaks <- length(peaks$positions)
  prominences <- numeric(n_peaks)

  for (i in seq_along(peaks$positions)) {
    peak_pos <- peaks$positions[i]
    peak_height <- peaks$heights[i]

    # Left-side minimum: Scan leftward until you find a higher peak or the edge of the data
    left_min <- peak_height
    if (peak_pos > 1) {
      for (j in seq(peak_pos - 1, 1)) {
        if (data[j] >= peak_height) {
          break
        }
        left_min <- min(left_min, data[j])
      }
    }

    # Right-side minimum: Scan rightward until you find a higher peak or the edge of the data
    right_min <- peak_height
    if (peak_pos < length(data)) {
      for (j in seq(peak_pos + 1, length(data))) {
        if (data[j] >= peak_height) {
          break
        }
        right_min <- min(right_min, data[j])
      }
    }

    # Use the smaller of the two minima (closer to the peak)
    closer_min <- min(left_min, right_min)

    # Prominence is the peak height minus the closer minimum
    prominences[i] <- peak_height - closer_min

    # Optional Debugging (can be removed in production)
    cat("Peak at position", peak_pos, ": Left Min =", left_min,
        ", Right Min =", right_min, ", Closer Min =", closer_min,
        ", Peak Height =", peak_height, "\n")
    cat("Calculated prominence for peak at position", peak_pos, "=", prominences[i], "\n")
  }

  # Return a Prominence object containing positions and calculated prominences
  return(Prominence(positions = peaks$positions, prominences = prominences))
}
