#' Peak Class Constructor
#' Creates a Peak object containing peak positions and heights.
#' @param positions Numeric vector of peak positions.
#' @param heights Numeric vector of peak heights.
#' @return A Peak object.
#' @export
.Peak <- function(positions, heights) {
  structure(list(positions = positions, heights = heights), class = "Peak")
}

#' Print Method for Peak Class
#' @param x A Peak object.
#' @param ... Additional parameters.
#' @export
print.Peak <- function(x, ...) {
  cat("Detected Peaks:\n")
  cat("Positions: ", x$positions, "\n")
  cat("Heights: ", x$heights, "\n")
}

#' Summary Method for Peak Class
#' @param object A Peak object.
#' @param ... Additional parameters.
#' @export
summary.Peak <- function(object, ...) {
  cat("Summary of Detected Peaks:\n")
  cat("Number of Peaks:", length(object$positions), "\n")
  cat("Average Peak Height:", mean(object$heights), "\n")
}
