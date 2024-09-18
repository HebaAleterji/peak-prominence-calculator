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

#' Print method for Peak objects
#' @param x A Peak object.
#' @param ... Additional arguments (ignored).
#' @export
print.Peak <- function(x, ...) {
  cat("Detected Peaks:\n")
  print(data.frame(positions = x$positions, heights = x$heights))
}

#' Summary method for Peak objects
#' @param object A Peak object.
#' @param ... Additional arguments (ignored).
#' @export
summary.Peak <- function(object, ...) {
  cat("Summary of Detected Peaks:\n")
  cat("Number of Peaks:", length(object$positions), "\n")
  cat("Average Peak Height:", mean(object$heights), "\n")

}

