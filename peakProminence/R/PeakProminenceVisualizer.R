#' Coercion to PeakProminenceVisualizer Object
#' Converts numeric data to a PeakProminenceVisualizer object for visualization.
#' @param data Numeric vector of data points.
#' @return A PeakProminenceVisualizer object.
#' @export
as.peakVisualizer <- function(data) {
  # Ensure data is numeric
  if (!is.numeric(data)) {
    stop("data must be a numeric vector")
  }

  # Detect peaks and calculate prominence
  peaks <- detect_peaks(data)
  prominence <- calculate_prominence(peaks, data)

  # Create and return the PeakProminenceVisualizer object
  structure(list(
    data = data,
    peaks = peaks,
    prominence = prominence
  ), class = "PeakProminenceVisualizer")
}

#' Plot Peaks for PeakProminenceVisualizer
#' Visualizes the detected peaks on the original data.
#' @param object A PeakProminenceVisualizer object.
#' @export
plot_peaks <- function(object) {
  if (!inherits(object, "PeakProminenceVisualizer")) {
    stop("Object is not of class 'PeakProminenceVisualizer'")
  }

  data <- object$data
  peaks <- object$peaks

  # Plot the data
  plot(data, type = "l", main = "Peak Detection", xlab = "Index", ylab = "Value", col = "black")

  # Highlight the peaks
  points(peaks$positions, peaks$heights, col = "red", pch = 19)
}

#' Plot Prominence for PeakProminenceVisualizer
#' Visualizes the prominence of detected peaks on the original data.
#' @param object A PeakProminenceVisualizer object.
#' @export
plot_prominence <- function(object) {
  if (!inherits(object, "PeakProminenceVisualizer")) {
    stop("Object is not of class 'PeakProminenceVisualizer'")
  }

  data <- object$data
  peaks <- object$peaks
  prominences <- object$prominence

  # Plot the data
  plot(data, type = "l", main = "Prominence Detection", xlab = "Index", ylab = "Value", col = "black")

  # Highlight the peaks
  points(peaks$positions, peaks$heights, col = "red", pch = 19)

  # Add lines showing the prominence
  for (i in seq_along(peaks$positions)) {
    segments(x0 = peaks$positions[i],
             y0 = peaks$heights[i] - prominences$prominences[i],
             x1 = peaks$positions[i],
             y1 = peaks$heights[i],
             col = "blue", lwd = 2)
  }
}
