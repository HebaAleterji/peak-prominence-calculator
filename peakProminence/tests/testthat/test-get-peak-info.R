# test-get-peak-info.R

test_that("get_peak_info returns correct data frame", {
  peaks <- Peak(positions = c(3, 6), heights = c(12, 14))
  prominence <- Prominence(positions = c(3, 6), prominences = c(5, 10))

  peak_info <- get_peak_info(peaks, prominence)

  expect_equal(ncol(peak_info), 3)
  expect_equal(peak_info$PeakPosition, c(3, 6))
  expect_equal(peak_info$PeakHeight, c(12, 14))
  expect_equal(peak_info$Prominence, c(5, 10))
})
