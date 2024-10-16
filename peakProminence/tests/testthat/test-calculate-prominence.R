# Load required packages
library(testthat)

# Test calculate_prominence for 1D data
test_that("calculate_prominence correctly calculates prominence for 1D data", {
  data <- c(1, 7, 1, 7, 1, 1, 9, 1, 1, 10, 1)

  # Detect peaks
  peaks <- detect_peaks(data)

  # Calculate prominence
  result_prominence <- calculate_prominence(peaks, data)

  # Expected prominence values based on the logic of the function
  expected_prominence <- c(6, 6, 8, 9)  # Calculated as peak height - max(left_min, right_min)

  expect_equal(result_prominence$prominences, expected_prominence)
})

# Test calculate_prominence for 1D data with no peaks
test_that("calculate_prominence handles cases with no peaks for 1D data", {
  data <- c(1, 1, 1, 1, 1)  # No significant peaks

  peaks <- detect_peaks(data)
  result_prominence <- calculate_prominence(peaks, data)

  # Check if prominences are correctly returned as empty
  expect_equal(result_prominence$prominences, numeric(0))
})

# Test calculate_prominence for edge cases in 1D data
test_that("calculate_prominence handles edge cases in 1D data", {
  data <- c(1, 7, 1, 7, 1)  # Peaks with identical heights

  peaks <- detect_peaks(data)
  result_prominence <- calculate_prominence(peaks, data)

  # Expected prominence values based on the peak heights and local minima
  expected_prominence <- c(6, 6)

  expect_equal(result_prominence$prominences, expected_prominence)
})

