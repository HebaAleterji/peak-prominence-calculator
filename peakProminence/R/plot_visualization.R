#' Plot Detected Peaks for 1D or 2D Data
#' Visualizes the detected peaks for the entire data.
#' @param peaks A PeakData object (for 1D data) or a list of PeakData objects (for 2D data).
#' @param data A numeric vector (1D) or matrix (2D) representing the data.
#' @param main_title Title of the plot. Defaults to "Peak Detection".
#' @param peak_color Color for the peaks. Defaults to "red".
#' @param line_color Color for the line plot. Defaults to "black".
#' @export
plot_peaks <- function(peaks, data,
                       main_title = "Peak Detection",
                       peak_color = "red",
                       line_color = "black") {
  if (is.vector(data) && is.numeric(data)) {
    # Handle 1D data
    plot(data, type = "l", main = main_title, xlab = "Index", ylab = "Value", col = line_color)
    points(peaks$positions, peaks$heights, col = peak_color, pch = 19)

  } else if (is.matrix(data)) {
    # Handle 2D data row-wise
    par(mfrow = c(ceiling(sqrt(nrow(data))), ceiling(sqrt(nrow(data)))))  # Create an adaptive grid for plotting
    for (i in 1:nrow(data)) {
      plot(data[i, ], type = "l", main = paste(main_title, "- Row", i), xlab = "Index", ylab = "Value", col = line_color)
      points(peaks[[i]]$positions, peaks[[i]]$heights, col = peak_color, pch = 19)
    }
  } else {
    stop("Data must be either a numeric vector (1D) or a numeric matrix (2D).")
  }
}
#' Plot Peak Prominence for 1D or 2D Data
#' Visualizes the prominence of detected peaks for the entire data.
#' @param peaks A PeakData object (for 1D data) or a list of PeakData objects (for 2D data).
#' @param prominence A Prominence object (for 1D data) or a list of Prominence objects (for 2D data).
#' @param data A numeric vector (1D) or matrix (2D) representing the data.
#' @param main_title Title of the plot. Defaults to "Prominence Detection".
#' @param peak_color Color for the peaks. Defaults to "red".
#' @param line_color Color for the line plot. Defaults to "black".
#' @param prominence_color Color for the prominence bars. Defaults to "blue".
#' @export
plot_prominence <- function(peaks, prominence, data,
                            main_title = "Prominence Detection",
                            peak_color = "red",
                            line_color = "black",
                            prominence_color = "blue") {
  if (is.vector(data) && is.numeric(data)) {
    # Handle 1D data
    plot(data, type = "l", main = main_title, xlab = "Index", ylab = "Value", col = line_color)
    points(peaks$positions, peaks$heights, col = peak_color, pch = 19)

    # Add lines showing the prominence (down to the correct valley)
    for (i in seq_along(peaks$positions)) {
      valley_height <- peaks$heights[i] - prominence$prominences[i]
      segments(x0 = peaks$positions[i],
               y0 = valley_height,  # Go down to the correct valley, not the baseline
               x1 = peaks$positions[i],
               y1 = peaks$heights[i],
               col = prominence_color, lwd = 2)
    }

  } else if (is.matrix(data)) {
    # Handle 2D data row-wise
    par(mfrow = c(ceiling(sqrt(nrow(data))), ceiling(sqrt(nrow(data)))))  # Create an adaptive grid for plotting
    for (i in 1:nrow(data)) {
      plot(data[i, ], type = "l", main = paste(main_title, "- Row", i), xlab = "Index", ylab = "Value", col = line_color)
      points(peaks[[i]]$positions, peaks[[i]]$heights, col = peak_color, pch = 19)

      # Add lines showing the prominence for 2D data (corrected to the valley)
      for (j in seq_along(peaks[[i]]$positions)) {
        valley_height <- peaks[[i]]$heights[j] - prominence[[i]]$prominences[j]
        segments(x0 = peaks[[i]]$positions[j],
                 y0 = valley_height,  # Correctly plot to the valley
                 x1 = peaks[[i]]$positions[j],
                 y1 = peaks[[i]]$heights[j],
                 col = prominence_color, lwd = 2)
      }
    }
  } else {
    stop("Data must be either a numeric vector (1D) or a numeric matrix (2D).")
  }
}
