#' PeakVisualizer Class Constructor
#' Creates a PeakVisualizer object to handle peak and prominence visualization.
#' @param data Numeric vector of data points.
#' @return A PeakVisualizer object.
#' @export
PeakVisualizer <- function(data) {
  peaks <- detect_peaks(data)
  prominence <- calculate_prominence(peaks, data)
  structure(list(data = data, peaks = peaks, prominence = prominence), class = "PeakVisualizer")
}

#' Plot Peaks for PeakVisualizer
#' @param object A PeakVisualizer object.
#' @export
plot_peaks <- function(object) {
  data <- object$data
  peaks <- object$peaks

  # Plot the data
  plot(data, type = "l", main = "Peak Detection", xlab = "Index", ylab = "Value")

  # Highlight the peaks
  points(peaks$positions, peaks$heights, col = "red", pch = 19)
}

#' Plot Prominence for PeakVisualizer
#' @param object A PeakVisualizer object.
#' @export
plot_prominence <- function(object) {
  data <- object$data
  peaks <- object$peaks
  prominences <- object$prominence

  # Plot the data
  plot(data, type = "l", main = "Prominence Detection", xlab = "Index", ylab = "Value")

  # Highlight the peaks
  points(peaks$positions, peaks$heights, col = "red", pch = 19)

  # Add lines showing the prominence
  for (i in 1:length(peaks$positions)) {
    segments(x0 = peaks$positions[i], y0 = peaks$heights[i] - prominences$prominences[i],
             x1 = peaks$positions[i], y1 = peaks$heights[i], col = "blue", lwd = 2)
  }
}
