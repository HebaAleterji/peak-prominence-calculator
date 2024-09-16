# R/peak_class.R

#' Create a Peak object
#'
#' @param positions A numeric vector of peak positions.
#' @param heights A numeric vector of peak heights.
#' @return An object of class Peak containing the positions and heights of detected peaks.
#' @export
Peak <- function(positions, heights) {
  structure(list(positions = positions, heights = heights), class = "Peak")
}

#' Print a Peak object
#'
#' @param obj A Peak object.
#' @export
print.Peak <- function(obj) {
  cat("Detected Peaks\n")
  cat("Positions:", paste(obj$positions, collapse = " "), "\n")
  cat("Heights:", paste(obj$heights, collapse = " "), "\n")
}

#' Summary of a Peak object
#'
#' @param obj A Peak object.
#' @export
summary.Peak <- function(obj) {
  cat("Summary of Detected Peaks:\n")
  cat("Number of Peaks:", length(obj$positions), "\n")
  cat("Average Peak Height:", mean(obj$heights), "\n")
  cat("Peak Positions:", paste(obj$positions, collapse = " "), "\n")
  cat("Peak Heights:", paste(obj$heights, collapse = " "), "\n")
}
