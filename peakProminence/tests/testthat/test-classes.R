

library(testthat)

# Test Peak class
test_that("Peak class works as expected", {
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))

  # Check print output
  expect_output(print(peaks), "Detected Peaks")

  # Check summary output
  expect_output(summary(peaks), "Summary of Detected Peaks")
  expect_output(summary(peaks), "Number of Peaks: 2")
  expect_output(summary(peaks), "Average Peak Height: 13")
})

# Test Prominence class
test_that("Prominence class works as expected", {
  prominence <- Prominence(positions = c(3, 6), prominences = c(5, 10))

  # Check print output
  expect_output(print(prominence), "Calculated Prominence")

  # Check summary output
  expect_output(summary(prominence), "Summary of Peak Prominences")
  expect_output(summary(prominence), "Number of Peaks: 2")
  expect_output(summary(prominence), "Average Prominence: 7.5")
})

# Test calculate_prominence function
test_that("calculate_prominence correctly calculates prominence", {
  data <- c(1, 3, 12, 7, 8, 14, 2)
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))

  # Calculate prominence
  result_prominence <- calculate_prominence(data, peaks)

  # Check prominence values (update expected values if necessary)
  expect_equal(result_prominence$prominences, c(10, 12))  # Adjust these if correct values
})

# Test PeakVisualizer for peaks
test_that("PeakVisualizer works as expected for peaks", {
  data <- c(1, 3, 12, 7, 8, 14, 2)
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))

  visualizer <- PeakVisualizer(data = data, peaks = peaks)

  # Check plot for peaks
  expect_silent(plot_peaks(visualizer))
})

# Test PeakVisualizer for prominence
test_that("PeakVisualizer works as expected for prominence", {
  data <- c(1, 3, 12, 7, 8, 14, 2)
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))
  prominence <- Prominence(positions = c(3, 6), prominences = c(5, 10))

  visualizer <- PeakVisualizer(data = data, peaks = peaks, prominence = prominence)

  # Check plot for prominence
  expect_silent(plot_prominence(visualizer))
})
