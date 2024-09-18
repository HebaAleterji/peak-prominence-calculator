# R/peak_visualizer_class.R

#' Plot detected peaks using PeakVisualizer
#' @param obj A PeakVisualizer object.
#' @export
plot_peaks.PeakVisualizer <- function(obj) {
  if (is.null(obj$peaks)) stop("No peaks detected.")

  # Plot the data
  plot(obj$data, type = "l", main = "Detected Peaks", xlab = "Index", ylab = "Value")

  # Plot the peaks using graphics::points to ensure correct import
  graphics::points(obj$peaks$positions, obj$peaks$heights, col = "red", pch = 19)
}

#' Plot peak prominence using PeakVisualizer
#' @param obj A PeakVisualizer object.
#' @export
plot_prominence.PeakVisualizer <- function(obj) {
  if (is.null(obj$prominence)) stop("No prominence data.")

  # Plot the prominence using graphics::barplot to ensure correct import
  graphics::barplot(obj$prominence$prominences, names.arg = obj$prominence$positions,
                    main = "Peak Prominence", xlab = "Position", ylab = "Prominence", col = "blue")
}
