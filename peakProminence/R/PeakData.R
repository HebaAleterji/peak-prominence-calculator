#' Coercion to PeakData Object
#' Converts numeric data to a PeakData object.
#' @param positions Numeric vector of peak positions.
#' @param heights Numeric vector of peak heights.
#' @return A PeakData object.
#' @export
as.peak <- function(positions, heights) {
  # Ensure positions and heights are numeric vectors and of equal length
  if (!is.numeric(positions) || !is.numeric(heights)) {
    stop("positions and heights must be numeric vectors")
  }
  if (length(positions) != length(heights)) {
    stop("positions and heights must have the same length")
  }

  # Create and return the PeakData object
  structure(list(
    positions = positions,
    heights = heights
  ), class = "PeakData")
}

#' Print Method for PeakData Class
#' @param x A PeakData object.
#' @param ... Additional parameters (currently unused).
#' @export
print.PeakData <- function(x, ...) {
  if (!inherits(x, "PeakData")) {
    stop("Object is not of class 'PeakData'")
  }
  # Handle unused arguments (if any are passed)
  if (length(list(...)) > 0) {
    warning("Additional arguments ignored in print.PeakData.")
  }

  cat("Detected Peaks:\n")
  cat("Positions: ", paste(x$positions, collapse = ", "), "\n")
  cat("Heights: ", paste(x$heights, collapse = ", "), "\n")
}

#' Summary Method for PeakData Class
#' @param object A PeakData object.
#' @param ... Additional parameters (currently unused).
#' @export
summary.PeakData <- function(object, ...) {
  if (!inherits(object, "PeakData")) {
    stop("Object is not of class 'PeakData'")
  }
  # Handle unused arguments (if any are passed)
  if (length(list(...)) > 0) {
    warning("Additional arguments ignored in summary.PeakData.")
  }

  cat("Summary of Detected Peaks:\n")
  cat("Number of Peaks:", length(object$positions), "\n")
  cat("Average Peak Height:", mean(object$heights), "\n")
}

