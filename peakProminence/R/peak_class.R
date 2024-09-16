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
#' This function prints the peak positions and heights stored in a Peak object.
#'
#' @param obj A Peak object.
#' @export
print.Peak <- function(obj) {
  cat("Detected Peaks\n")
  cat("Positions:", obj$positions, "\n")
  cat("Heights:", obj$heights, "\n")
}

#' Summary of a Peak object
#'
#' This function provides a summary of a Peak object, including the number of peaks,
#' average peak height, and details about the positions and heights.
#'
#' @param obj A Peak object.
#' @export
summary.Peak <- function(obj) {
  cat("Summary of Detected Peaks:\n")
  cat("Number of Peaks:", length(obj$positions), "\n")
  cat("Average Peak Height:", mean(obj$heights), "\n")
  cat("Peak Positions:", obj$positions, "\n")
  cat("Peak Heights:", obj$heights, "\n")
}
