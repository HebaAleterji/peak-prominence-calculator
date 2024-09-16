# R/peak_visualizer_class.R

#' Create a PeakVisualizer object
#'
#' This object is responsible for visualizing peaks and their prominence.
#'
#' @param data A numeric vector (1D) or matrix (2D) representing the signal data.
#' @param peaks A Peak object that contains detected peak positions and heights.
#' @param prominence (Optional) A Prominence object with calculated prominence values.
#' @return An object of class PeakVisualizer for visualizing the peaks and prominence.
#' @export
PeakVisualizer <- function(data, peaks, prominence = NULL) {
  structure(list(data = data, peaks = peaks, prominence = prominence), class = "PeakVisualizer")
}

#' Plot detected peaks using the PeakVisualizer
#'
#' @param obj A PeakVisualizer object.
#' @export
plot_peaks.PeakVisualizer <- function(obj) {
  if (is.null(obj$peaks)) {
    stop("No peaks detected. Please provide a Peak object.")
  }

  # Plot the original data
  plot(obj$data, type = "l", main = "Detected Peaks", xlab = "Index", ylab = "Value")

  # Add the detected peaks to the plot
  points(obj$peaks$positions, obj$peaks$heights, col = "red", pch = 19)
  legend("topright", legend = "Peaks", col = "red", pch = 19)
}

#' Plot prominence values using the PeakVisualizer
#'
#' @param obj A PeakVisualizer object.
#' @export
plot_prominence.PeakVisualizer <- function(obj) {
  if (is.null(obj$prominence)) {
    stop("No prominence data available. Please provide a Prominence object.")
  }

  # Plot the prominence values as a bar plot
  barplot(obj$prominence$prominences, names.arg = obj$prominence$positions,
          main = "Peak Prominence", xlab = "Peak Position", ylab = "Prominence", col = "blue")
}
