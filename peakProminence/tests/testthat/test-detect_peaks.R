# Test detect_peaks function
test_that("detect_peaks correctly identifies peaks in data", {
  data <- c(1, 3, 7, 1, 2, 5, 9, 3, 1, 10, 3)

  # Expected peaks at indices 3, 7, and 10 with heights 7, 9, and 10
  result_peaks <- detect_peaks(data)

  expect_equal(sort(result_peaks$positions), c(3, 7, 10))
  expect_equal(sort(result_peaks$heights), c(7, 9, 10))
})

test_that("detect_peaks handles empty input", {
  data <- numeric(0)

  result_peaks <- detect_peaks(data)

  expect_equal(result_peaks$positions, numeric(0))
  expect_equal(result_peaks$heights, numeric(0))
})
