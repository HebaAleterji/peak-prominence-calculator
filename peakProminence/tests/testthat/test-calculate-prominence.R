# Load required packages
library(testthat)

# Helper function to print prominence for debugging
print_prominence <- function(prominence) {
  cat("Calculated Prominence:\n")
  cat("Positions:", prominence$positions, "\n")
  cat("Prominences:", prominence$prominences, "\n")
}

# Test 1: Correct calculation of prominence for multiple peaks in 1D data
test_that("calculate_prominence correctly calculates prominence for 1D data", {
  data <- c(1, 7, 1, 7, 1, 1, 9, 1, 1, 10, 1)

  # Detect peaks
  peaks <- detect_peaks(data)

  # Calculate prominence
  result_prominence <- calculate_prominence(peaks, data)

  # Print results for debugging
  print_prominence(result_prominence)

  # Adjusted expected prominence values to match the correct detection logic
  expected_prominence <- c(9, 8, 6, 6)  # Order: Positions 10, 7, 2, 4

  expect_equal(result_prominence$prominences, expected_prominence,
               info = "Prominence values should match the expected calculation.")
})

# Test 2: Handling 1D data with no peaks
test_that("calculate_prominence handles cases with no peaks for 1D data", {
  data <- c(1, 1, 1, 1, 1)  # Flat data with no peaks

  # Detect peaks
  peaks <- detect_peaks(data)

  # Calculate prominence
  result_prominence <- calculate_prominence(peaks, data)

  # Check for empty prominence
  expect_equal(result_prominence$prominences, numeric(0),
               info = "Prominence should be empty for data with no peaks.")
})

# Test 3: Handling edge cases with identical peak heights
test_that("calculate_prominence handles edge cases in 1D data", {
  data <- c(1, 7, 1, 7, 1)  # Repeating peaks with identical heights

  # Detect peaks
  peaks <- detect_peaks(data)

  # Calculate prominence
  result_prominence <- calculate_prominence(peaks, data)

  # Adjusted expected prominence values for identical peaks
  expected_prominence <- c(6, 6)

  expect_equal(result_prominence$prominences, expected_prominence,
               info = "Prominence values should match for identical peaks.")
})
