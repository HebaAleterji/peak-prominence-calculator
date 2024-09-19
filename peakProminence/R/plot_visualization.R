#' Plot Peaks
#' @param object A PeakVisualizer object.
#' @export
plot_peaks.PeakVisualizer <- function(object) {
  data <- object$data
  peaks <- object$peaks

  # Plot the data
  plot(data, type = "l", main = "Peak Detection", xlab = "Index", ylab = "Value")

  # Highlight the peaks
  points(peaks$positions, peaks$heights, col = "red", pch = 19)
}

#' Plot Prominence
#' @param object A PeakVisualizer object.
#' @export
plot_prominence.PeakVisualizer <- function(object) {
  data <- object$data
  peaks <- object$peaks
  prominences <- object$prominence

  # Plot the data
  plot(data, type = "l", main = "Prominence Detection", xlab = "Index", ylab = "Value")

  # Highlight the peaks
  points(peaks$positions, peaks$heights, col = "red", pch = 19)

  # Add barplot for prominences
  for (i in 1:length(peaks$positions)) {
    segments(x0 = peaks$positions[i], y0 = peaks$heights[i] - prominences$prominences[i],
             x1 = peaks$positions[i], y1 = peaks$heights[i], col = "blue", lwd = 2)
  }
}
