# File: tests/testthat/test-classes.R

library(testthat)

# Sample data for tests
data <- c(1, 3, 12, 7, 8, 14, 2)

# Test for Peak class
test_that("Peak class works as expected", {
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))

  # Check that the object is of class Peak
  expect_s3_class(peaks, "Peak")

  # Test print method
  expect_output(print(peaks), "Detected Peaks")

  # Test summary method
  expect_output(summary(peaks), "Peak Position: 3 6")
})

# Test for Prominence class
test_that("Prominence class works as expected", {
  prominence <- Prominence(positions = c(3, 6), prominences = c(5, 10))

  # Check that the object is of class Prominence
  expect_s3_class(prominence, "Prominence")

  # Test print method
  expect_output(print(prominence), "Calculated Prominence")

  # Test summary method
  expect_output(summary(prominence), "Prominences: 5 10")
})

# Test for detect_peaks function
test_that("detect_peaks correctly identifies peaks in 2D data", {
  result_peaks <- detect_peaks(data, min_peak_height = 5)

  # Check positions and heights
  expect_equal(result_peaks$positions, c(3, 6))
  expect_equal(result_peaks$heights, c(12, 14))
})

# Test for calculate_prominence function
test_that("calculate_prominence correctly calculates prominence", {
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))
  result_prominence <- calculate_prominence(data, peaks)

  # Check positions and prominences
  expect_equal(result_prominence$positions, c(3, 6))
  expect_equal(result_prominence$prominences, c(9, 10))  # example expected values
})

# Test for PeakVisualizer class and plot_peaks function
test_that("PeakVisualizer works as expected for peaks", {
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))
  visualizer <- PeakVisualizer(data = data, peaks = peaks)

  # Test the class and plot function
  expect_silent(plot_peaks(visualizer))
})

# Test for PeakVisualizer class and plot_prominence function
test_that("PeakVisualizer works as expected for prominence", {
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))
  prominence <- Prominence(positions = c(3, 6), prominences = c(5, 10))
  visualizer <- PeakVisualizer(data = data, peaks = peaks, prominence = prominence)

  # Test the class and plot function
  expect_silent(plot_prominence(visualizer))
})

# Test for get_peak_info function
test_that("get_peak_info correctly combines peaks and prominences", {
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))
  prominence <- Prominence(positions = c(3, 6), prominences = c(5, 10))

  result <- get_peak_info(peaks, prominence)

  # Check the combined data frame
  expect_equal(result$PeakPosition, c(3, 6))
  expect_equal(result$PeakHeight, c(12, 14))
  expect_equal(result$Prominence, c(5, 10))
})
