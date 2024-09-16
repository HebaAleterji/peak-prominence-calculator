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

#' Print a Prominence object
#'
#' This function prints the prominence values for each peak.
#'
#' @param obj A Prominence object.
#' @export
print.Prominence <- function(obj) {
  cat("Calculated Prominence\n")
  cat("Positions:", paste(obj$positions, collapse = " "), "\n")
  cat("Prominence:", paste(obj$prominences, collapse = " "), "\n")
}

#' Summary of a Prominence object
#'
#' @param obj A Prominence object.
#' @export
summary.Prominence <- function(obj) {
  cat("Summary of Peak Prominences:\n")
  cat("Number of Peaks with Prominence:", length(obj$positions), "\n")
  cat("Average Prominence Value:", mean(obj$prominences), "\n")
  cat("Peak Positions:", paste(obj$positions, collapse = " "), "\n")
  cat("Prominence:", paste(obj$prominences, collapse = " "), "\n")
}
