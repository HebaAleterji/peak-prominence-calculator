# R/peak_info.R

#' Get combined peak information as a data frame
#' @param peaks A Peak object.
#' @param prominence A Prominence object.
#' @return A data frame with Peak Position, Peak Height, and Prominence.
#' @export
get_peak_info <- function(peaks, prominence) {
  if (is.null(peaks) || is.null(prominence)) stop("Peaks or prominence data missing.")

  peak_info <- data.frame(
    PeakPosition = peaks$positions,
    PeakHeight = peaks$heights,
    Prominence = prominence$prominences
  )

  return(peak_info)
}
