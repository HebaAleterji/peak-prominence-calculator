# Test for plot_peaks visualization function
test_that("plot_peaks works correctly for 1D data", {
  data <- c(1, 7, 1, 7, 1, 1, 9, 1, 1, 10, 1)
  peaks <- detect_peaks(data)

  # Test that the function runs without errors
  expect_silent(plot_peaks(peaks = peaks, data = data))
})

# Test for plot_prominence visualization function
test_that("plot_prominence works correctly for 1D data", {
  data <- c(1, 7, 1, 7, 1, 1, 9, 1, 1, 10, 1)
  peaks <- detect_peaks(data)
  prominence <- calculate_prominence(peaks, data)

  # Test that the function runs without errors
  expect_silent(plot_prominence(peaks = peaks, prominence = prominence, data = data))
})

# Test for 2D data
test_that("plot_peaks and plot_prominence work correctly for 2D data", {
  data_2d <- matrix(c(1, 7, 1, 7, 1, 1, 9, 1, 1, 10, 1,
                      2, 6, 2, 8, 2, 2, 10, 2, 2, 12, 2),
                    nrow = 2, byrow = TRUE)

  # Detect peaks and calculate prominence for 2D data
  peaks_2d <- detect_peaks(data_2d)
  prominence_2d <- calculate_prominence(peaks_2d, data_2d)

  # Test that the functions run without errors for 2D data
  expect_silent(plot_peaks(peaks = peaks_2d, data = data_2d))
  expect_silent(plot_prominence(peaks = peaks_2d, prominence = prominence_2d, data = data_2d))
})
