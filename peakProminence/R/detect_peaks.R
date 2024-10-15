#' Detect Peaks in 2D Data
#' This function detects peaks in a 2D numeric matrix and filters out noise.
#' Peaks are detected either row-wise or column-wise, depending on the user's choice.
#' @param data A numeric matrix representing the 2D data.
#' @param noise_threshold A numeric value specifying the minimum height a peak
#'        must have to be considered significant. Defaults to 0 (no filtering).
#' @param min_distance An integer specifying the minimum distance between peaks.
#'        This helps avoid detecting multiple close peaks in noisy regions. Defaults to 1.
#' @param direction Character string indicating the direction for peak detection,
#'        either "row" for row-wise detection or "column" for column-wise detection.
#'        Defaults to "row".
#' @return A list of PeakData objects for each row or column, depending on the direction.
#' @export
detect_peaks <- function(data, noise_threshold = 0, min_distance = 1, direction = "row") {
  # Ensure data is a matrix
  if (!is.matrix(data)) {
    stop("Data must be a numeric matrix.")
  }

  # Validate direction parameter
  if (!direction %in% c("row", "column")) {
    stop("Direction must be either 'row' or 'column'.")
  }

  # Define peak detection function for individual vectors
  detect_single_peaks <- function(vector) {
    peaks <- pracma::findpeaks(vector,
                               minpeakheight = noise_threshold,
                               minpeakdistance = min_distance,
                               sortstr = TRUE)
    if (is.null(peaks)) {
      return(as.peak(positions = numeric(0), heights = numeric(0)))
    }
    positions <- peaks[, 2]
    heights <- peaks[, 1]
    as.peak(positions = positions, heights = heights)
  }

  # Detect peaks row-wise or column-wise
  if (direction == "row") {
    result <- apply(data, 1, detect_single_peaks)  # Apply row-wise
  } else {
    result <- apply(data, 2, detect_single_peaks)  # Apply column-wise
  }

  return(result)
}
