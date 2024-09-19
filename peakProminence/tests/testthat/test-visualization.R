# Test for plot_peaks visualization function
test_that("plot_peaks works correctly", {
  data <- c(1, 7, 1, 7, 1, 1, 9, 1, 1, 10, 1)
  peaks <- detect_peaks(data)

  # Create PeakVisualizer object
  visualizer <- PeakVisualizer(data = data)

  # Test that the function runs without errors
  expect_silent(plot_peaks(visualizer))
})

# Test for plot_prominence visualization function
test_that("plot_prominence works correctly", {
  data <- c(1, 7, 1, 7, 1, 1, 9, 1, 1, 10, 1)
  peaks <- detect_peaks(data)
  prominence <- calculate_prominence(peaks, data)

  # Create PeakVisualizer object
  visualizer <- PeakVisualizer(data = data)

  # Test that the function runs without errors
  expect_silent(plot_prominence(visualizer))
})
