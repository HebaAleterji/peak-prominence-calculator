

#' Calculate Prominence for Peaks
#' @param data A numeric vector representing the 2D data.
#' @param peaks A Peak object containing detected peaks.
#' @return A Prominence object containing the prominences of each peak.
#' @export
calculate_prominence <- function(data, peaks) {
  prominences <- numeric(length(peaks$positions))

  for (i in seq_along(peaks$positions)) {
    pos <- peaks$positions[i]
    left_min <- min(data[1:pos])
    right_min <- min(data[pos:length(data)])
    reference_level <- max(left_min, right_min)
    prominences[i] <- peaks$heights[i] - reference_level
  }

  Prominence(positions = peaks$positions, prominences = prominences)
}
