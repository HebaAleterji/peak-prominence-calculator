

#' PeakVisualizer class
#' @param data A numeric vector representing the signal data.
#' @param peaks A Peak object containing detected peaks.
#' @param prominence (Optional) A Prominence object containing prominence values for the peaks.
#' @return A PeakVisualizer object.
#' @export
PeakVisualizer <- function(data, peaks, prominence = NULL) {
  structure(list(data = data, peaks = peaks, prominence = prominence), class = "PeakVisualizer")
}
