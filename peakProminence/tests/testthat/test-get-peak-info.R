# Test for the main functionality of get_peak_info
test_that("get_peak_info returns correct data frame", {
  peaks <- as.peak(positions = c(3, 6), heights = c(12, 14))
  prominence <- as.prominence(positions = c(3, 6), prominences = c(5, 10))

  peak_info <- get_peak_info(peaks, prominence)

  # Check number of columns and correctness of data
  expect_equal(ncol(peak_info), 3)
  expect_equal(peak_info$PeakPosition, c(3, 6))
  expect_equal(peak_info$PeakHeight, c(12, 14))
  expect_equal(peak_info$Prominence, c(5, 10))
})

# Test for edge cases: empty peaks and prominence
test_that("get_peak_info handles empty peaks and prominence", {
  peaks <- as.peak(positions = numeric(0), heights = numeric(0))
  prominence <- as.prominence(positions = numeric(0), prominences = numeric(0))

  peak_info <- get_peak_info(peaks, prominence)

  # Ensure that the returned data frame is empty
  expect_equal(nrow(peak_info), 0)
  expect_equal(ncol(peak_info), 3)
  expect_equal(peak_info$PeakPosition, numeric(0))
  expect_equal(peak_info$PeakHeight, numeric(0))
  expect_equal(peak_info$Prominence, numeric(0))
})
