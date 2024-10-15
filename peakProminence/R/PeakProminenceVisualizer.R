#' PeakProminenceVisualizer for 2D Data
#' Creates a PeakProminenceVisualizer object to handle peak and prominence visualization.
#' @param data A numeric matrix representing the 2D data.
#' @return A list of PeakProminenceVisualizer objects for each row.
#' @export
as.peakVisualizer <- function(data) {
  # Ensure data is a matrix
  if (!is.matrix(data)) {
    stop("Data must be a numeric matrix.")
  }

  # Detect peaks and calculate prominence for each row
  peaks <- detect_peaks(data)
  prominence <- calculate_prominence(peaks, data)

  # Create a list of visualizer objects for each row
  visualizers <- lapply(1:nrow(data), function(i) {
    structure(list(
      data = data[i, ],
      peaks = peaks[[i]],
      prominence = prominence[[i]]
    ), class = "PeakProminenceVisualizer")
  })

  return(visualizers)
}
