

#' Plot Peaks
#' @param visualizer A PeakVisualizer object.
#' @export
plot_peaks.PeakVisualizer <- function(visualizer) {
  plot(visualizer$data, type = "l", main = "Peaks", xlab = "Index", ylab = "Value")
  points(visualizer$peaks$positions, visualizer$peaks$heights, col = "red", pch = 19)
}


#' Plot Prominences
#' @param visualizer A PeakVisualizer object with prominences.
#' @export
plot_prominence.PeakVisualizer <- function(visualizer) {
  plot(visualizer$data, type = "l", main = "Peaks and Prominences", xlab = "Index", ylab = "Value")
  points(visualizer$peaks$positions, visualizer$peaks$heights, col = "red", pch = 19)

  for (i in 1:length(visualizer$peaks$positions)) {
    lines(c(visualizer$peaks$positions[i], visualizer$peaks$positions[i]),
          c(visualizer$prominence$prominences[i], visualizer$peaks$heights[i]), col = "blue")
  }
}
