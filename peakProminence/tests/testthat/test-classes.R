# Load the required library
library(testthat)

# Test for detecting peaks
test_that("detect_peaks works as expected", {
  data <- c(1, 3, 7, 10, 9, 6, 3, 5, 10, 7, 2)

  # Detect peaks
  result_peaks <- detect_peaks(data)

  # Expected results
  expected_positions <- c(3, 9)
  expected_heights <- c(7, 10)

  # Compare detected peaks to expected results
  expect_equal(result_peaks$positions, expected_positions)
  expect_equal(result_peaks$heights, expected_heights)
})

# Test for calculate_prominence
test_that("calculate_prominence correctly calculates prominence", {
  data <- c(1, 3, 7, 10, 9, 6, 3, 5, 10, 7, 2)
  peaks <- detect_peaks(data)

  # Calculate prominence
  result_prominence <- calculate_prominence(peaks, data)

  # Expected prominence values
  expected_prominence <- c(4, 7)

  # Compare calculated prominence to expected results
  expect_equal(result_prominence$prominences, expected_prominence)
})

# Test for handling edge cases with calculate_prominence
test_that("calculate_prominence handles edge cases", {
  data <- c(1, 3, 7, 10, 9, 6, 3, 5, 10, 7, 2)
  peaks <- detect_peaks(data)

  # Calculate prominence
  result_prominence <- calculate_prominence(peaks, data)

  # Expected prominence values
  expected_prominence <- c(4, 7)

  # Compare calculated prominence to expected results
  expect_equal(result_prominence$prominences, expected_prominence)
})

# Test for PeakVisualizer
test_that("PeakVisualizer works as expected for peaks and prominence", {
  data <- c(1, 3, 7, 10, 9, 6, 3, 5, 10, 7, 2)

  # Create the PeakVisualizer object (detect peaks and prominence)
  visualizer <- PeakVisualizer(data = data)

  # Expected peaks and prominence
  expected_peaks_positions <- c(3, 9)
  expected_peaks_heights <- c(7, 10)
  expected_prominences <- c(4, 7)

  # Ensure the correct peaks and prominence are detected
  expect_equal(visualizer$peaks$positions, expected_peaks_positions)
  expect_equal(visualizer$peaks$heights, expected_peaks_heights)
  expect_equal(visualizer$prominence$prominences, expected_prominences)

  # Test the plotting functions (no errors expected)
  expect_silent(plot_peaks(visualizer))
  expect_silent(plot_prominence(visualizer))
})
