#' Detect Peaks in 1D or 2D Data using pracma::findpeaks
#'
#' This function wraps the findpeaks() function from the pracma package to identify peaks in a numeric vector or matrix.
#' For 2D data (matrix), it applies peak detection row-wise.
#'
#' @param data A numeric vector (1D) or matrix (2D) containing the data in which to detect peaks
#' @param min_peak_height A numeric value for the minimum peak height (default is 0)
#' @param apply_across Either "row" or "column" to specify if peaks should be detected row-wise or column-wise for 2D data (default is "row")
#' @return A list of data frames, each containing the positions and heights of detected peaks for each row or column in 2D data. For 1D data, returns a single data frame.
#' @import pracma
#' @export
detect_peaks <- function(data, min_peak_height = 0, apply_across = "row") {

  # Check if data is a numeric vector (1D) or matrix (2D)
  if (is.vector(data) && is.numeric(data)) {
    # For 1D data, apply findpeaks directly
    peaks <- pracma::findpeaks(data, minpeakheight = min_peak_height)
    if (is.null(peaks)) {
      return(data.frame(Position = integer(0), Height = numeric(0)))
    }
    return(data.frame(Position = peaks[, 2], Height = peaks[, 1]))

  } else if (is.matrix(data) && is.numeric(data)) {
    # For 2D data (matrix), apply peak detection row-wise or column-wise
    result_list <- list()

    if (apply_across == "row") {
      for (i in 1:nrow(data)) {
        row_peaks <- pracma::findpeaks(data[i, ], minpeakheight = min_peak_height)
        if (!is.null(row_peaks)) {
          result_list[[i]] <- data.frame(Row = i, Position = row_peaks[, 2], Height = row_peaks[, 1])
        } else {
          result_list[[i]] <- data.frame(Row = i, Position = integer(0), Height = numeric(0))
        }
      }
    } else if (apply_across == "column") {
      for (i in 1:ncol(data)) {
        col_peaks <- pracma::findpeaks(data[, i], minpeakheight = min_peak_height)
        if (!is.null(col_peaks)) {
          result_list[[i]] <- data.frame(Column = i, Position = col_peaks[, 2], Height = col_peaks[, 1])
        } else {
          result_list[[i]] <- data.frame(Column = i, Position = integer(0), Height = numeric(0))
        }
      }
    } else {
      stop("Invalid option for 'apply_across'. Use 'row' or 'column'.")
    }

    return(result_list)
  } else {
    stop("Input data must be either a numeric vector or matrix.")
  }

}
