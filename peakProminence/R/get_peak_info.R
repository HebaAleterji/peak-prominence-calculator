#' Get combined peak information as a data frame
#' Combines peak positions, heights, and prominence values into a single data frame.
#' @param peaks A PeakData object (or equivalent).
#' @param prominence A PeakProminence object (or equivalent).
#' @return A data frame with columns: Peak Position, Peak Height, and Prominence.
#' @export
get_peak_info <- function(peaks, prominence) {
  # Validate input
  if (is.null(peaks) || is.null(prominence)) {
    stop("Peaks or prominence data missing.")
  }

  # Ensure both inputs are of the correct class
  if (!inherits(peaks, "PeakData")) {
    stop("Peaks must be of class 'PeakData'.")
  }

  if (!inherits(prominence, "PeakProminence")) {
    stop("Prominence must be of class 'PeakProminence'.")
  }

  # Ensure positions and prominences have the same length
  if (length(peaks$positions) != length(prominence$prominences)) {
    stop("Mismatch: Peak positions and prominence lengths must be equal.")
  }

  # Create a data frame with combined information
  peak_info <- data.frame(
    PeakPosition = peaks$positions,
    PeakHeight = peaks$heights,
    Prominence = prominence$prominences
  )

  return(peak_info)
}
