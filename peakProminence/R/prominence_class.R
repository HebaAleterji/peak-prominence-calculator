#' Prominence Class Constructor
#' Creates a Prominence object.
#' @param positions Numeric vector of peak positions.
#' @param prominences Numeric vector of prominence values.
#' @return A Prominence object.
#' @export

# Constructor for the Prominence object
Prominence <- function(positions, prominences) {
  if (!is.numeric(positions) || !is.numeric(prominences)) {
    stop("positions and prominences must be numeric")
  }
  structure(list(
    positions = positions,
    prominences = prominences
  ), class = "Prominence")
}

#' Print Method for Prominence Class
#' @param x A Prominence object.
#' @param ... Additional parameters.
#' @export
print.Prominence <- function(x, ...) {
  cat("Calculated Prominences:\n")
  cat("Positions: ", x$positions, "\n")
  cat("Prominences: ", x$prominences, "\n")
}

#' Summary Method for Prominence Class
#' @param object A Prominence object.
#' @param ... Additional parameters.
#' @export
summary.Prominence <- function(object, ...) {
  cat("Summary of Peak Prominences:\n")
  cat("Number of Peaks:", length(object$positions), "\n")
  cat("Average Prominence:", mean(object$prominences), "\n")
}
