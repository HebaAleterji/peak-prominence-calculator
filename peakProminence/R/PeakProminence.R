#' Coercion to PeakProminence Object
#' Converts numeric data to a PeakProminence object.
#' @param positions Numeric vector of peak positions.
#' @param prominences Numeric vector of prominence values.
#' @return A PeakProminence object.
#' @export
as.prominence <- function(positions, prominences) {
  # Ensure positions and prominences are numeric vectors and of equal length
  if (!is.numeric(positions) || !is.numeric(prominences)) {
    stop("positions and prominences must be numeric vectors")
  }
  if (length(positions) != length(prominences)) {
    stop("positions and prominences must have the same length")
  }

  # Create and return the PeakProminence object
  structure(list(
    positions = positions,
    prominences = prominences
  ), class = "PeakProminence")
}

#' Print Method for PeakProminence Class
#' @param x A PeakProminence object.
#' @param ... Additional parameters.
#' @export
print.PeakProminence <- function(x, ...) {
  if (!inherits(x, "PeakProminence")) {
    stop("Object is not of class 'PeakProminence'")
  }
  cat("Calculated Prominences:\n")
  cat("Positions: ", paste(x$positions, collapse = ", "), "\n")
  cat("Prominences: ", paste(x$prominences, collapse = ", "), "\n")
}

#' Summary Method for PeakProminence Class
#' @param object A PeakProminence object.
#' @param ... Additional parameters.
#' @export
summary.PeakProminence <- function(object, ...) {
  if (!inherits(object, "PeakProminence")) {
    stop("Object is not of class 'PeakProminence'")
  }
  cat("Summary of Peak Prominences:\n")
  cat("Number of Peaks:", length(object$positions), "\n")
  cat("Average Prominence:", mean(object$prominences), "\n")
}
