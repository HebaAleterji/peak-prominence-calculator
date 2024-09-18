# R/prominence_class.R

#' Create a Prominence object
#'
#' @param positions A numeric vector of peak positions.
#' @param prominences A numeric vector of the prominence values for each peak.
#' @return An object of class Prominence containing the positions and prominence values of the peaks.
#' @export
Prominence <- function(positions, prominences) {
  structure(list(positions = positions, prominences = prominences), class = "Prominence")
}

#' Print method for Prominence objects
#' @param x A Prominence object.
#' @param ... Additional arguments (ignored).
#' @export
print.Prominence <- function(x, ...) {
  cat("Calculated Prominence:\n")
  print(data.frame(positions = x$positions, prominences = x$prominences))
}

#' Summary method for Prominence objects
#' @param object A Prominence object.
#' @param ... Additional arguments (ignored).
#' @export
summary.Prominence <- function(object, ...)  {
  cat("Summary of Peak Prominences:\n")
  cat("Number of Peaks:", length(object$positions), "\n")
  cat("Average Prominence:", mean(object$prominences), "\n")
}
