#' Calculate Prominence for Detected Peaks
#' The prominence is the vertical distance between the peak height and the reference level
#' (the higher minimum between the left and right boundaries).
#' @param peaks A PeakData object containing peak positions and heights.
#' @param data A numeric vector representing the original data.
#' @return A PeakProminence object with calculated prominence values for each peak.
#' @export
calculate_prominence <- function(peaks, data) {
  # Ensure the peaks object is not empty
  if (length(peaks$positions) == 0) {
    return(as.prominence(positions = numeric(0), prominences = numeric(0)))
  }

  n_peaks <- length(peaks$positions)
  prominences <- numeric(n_peaks)

  for (i in seq_along(peaks$positions)) {
    peak_pos <- peaks$positions[i]
    peak_height <- peaks$heights[i]

    # Left boundary (from the previous peak or start of data)
    if (i == 1) {
      left_min <- min(data[1:peak_pos])
    } else {
      left_min <- min(data[peaks$positions[i - 1]:peak_pos])
    }

    # Right boundary (up to the next peak or end of data)
    if (i == n_peaks) {
      right_min <- min(data[peak_pos:length(data)])
    } else {
      right_min <- min(data[peak_pos:peaks$positions[i + 1]])
    }

    # Prominence is the peak height minus the maximum of the left and right minima
    valley_min <- max(left_min, right_min)
    prominences[i] <- peak_height - valley_min

    # Debugging output (optional, for testing purposes)
    cat("Peak at position", peak_pos, ": Left Min =", left_min, ", Right Min =", right_min, ", Valley Min =", valley_min, ", Peak Height =", peak_height, "\n")
    cat("Calculated prominence for peak at position", peak_pos, "=", prominences[i], "\n")
  }

  # Return the calculated prominences as a PeakProminence object
  return(as.prominence(positions = peaks$positions, prominences = prominences))
}
