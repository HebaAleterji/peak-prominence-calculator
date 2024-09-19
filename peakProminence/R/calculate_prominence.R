
#' Calculate Prominence for Detected Peaks
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

    # Find the minimum to the left of the peak
    left_min <- if (peak_pos > 1) min(data[1:(peak_pos - 1)]) else peak_height

    # Find the minimum to the right of the peak
    right_min <- if (peak_pos < length(data)) min(data[(peak_pos + 1):length(data)]) else peak_height

    # Use the smaller of the two minima (closer to the peak)
    closer_min <- min(left_min, right_min)

    # Debugging: Print the left, right, and closer minima for each peak
    cat("Peak at position", peak_pos, ": Left Min =", left_min, ", Right Min =", right_min, ", Closer Min =", closer_min, ", Peak Height =", peak_height, "\n")

    # Prominence is the peak height minus the closer minimum
    prominences[i] <- peak_height - closer_min

    # Debugging: Print calculated prominence
    cat("Calculated prominence for peak at position", peak_pos, "=", prominences[i], "\n")
  }

  return(Prominence(positions = peaks$positions, prominences = prominences))
}
