
#' Detect Peaks in 2D Data
#' @param data A numeric vector representing the 2D data.
#' @param min_peak_height The minimum height a peak must have to be considered a peak.
#' @return A Peak object containing the positions and heights of the detected peaks.
#' @export
detect_peaks <- function(data, min_peak_height = 5) {
  peaks <- pracma::findpeaks(data, minpeakheight = min_peak_height)
  Peak(positions = peaks[, 2], heights = peaks[, 1])
}
