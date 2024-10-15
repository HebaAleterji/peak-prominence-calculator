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

    # Using Gaussian Mixture Model to estimate the baseline
    gmm_model <- Mclust(data, G = 2)  # Try modeling the data as two Gaussian distributions
    baseline <- min(gmm_model$parameters$mean)  # Use the mean of the smaller Gaussian component
    cat("Estimated baseline using GMM:", baseline, "\n")


  for (i in seq_along(peaks$positions)) {
    peak_pos <- peaks$positions[i]
    peak_height <- peaks$heights[i]

    # Find the local minimum between this peak and the next peak
    if (i == 1) {
      left_min <- min(data[1:peak_pos])
    } else {
      left_min <- min(data[peaks$positions[i - 1]:peak_pos])
    }

    if (i == n_peaks) {
      right_min <- min(data[peak_pos:length(data)])
    } else {
      right_min <- min(data[peak_pos:peaks$positions[i + 1]])
    }

    # Prominence is the peak height minus the maximum of the two minima or the baseline
    valley_min <- max(left_min, right_min, baseline)

    prominences[i] <- peak_height - valley_min

    # Debugging output
    cat("Peak at position", peak_pos, ": Left Min =", left_min, ", Right Min =", right_min, ", Valley Min =", valley_min, ", Peak Height =", peak_height, "\n")
    cat("Calculated prominence for peak at position", peak_pos, "=", prominences[i], "\n")
  }

  return(Prominence(positions = peaks$positions, prominences = prominences))
}
}

