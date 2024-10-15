# Test detect_peaks function for 1D data
test_that("detect_peaks correctly identifies peaks in 1D data", {
  data <- c(1, 3, 7, 1, 2, 5, 9, 3, 1, 10, 3)

  # Expected peaks at indices 3, 7, and 10 with heights 7, 9, and 10
  result_peaks <- detect_peaks(data, noise_threshold = 1)

  expect_equal(sort(result_peaks$positions), c(3, 7, 10))
  expect_equal(sort(result_peaks$heights), c(7, 9, 10))
})

# Test detect_peaks with adaptive noise threshold
test_that("detect_peaks adapts noise_threshold based on data", {
  data <- c(1, 3, 7, 1, 2, 5, 9, 3, 1, 10, 3)

  # Test adaptive noise threshold (automatically calculated based on data)
  result_peaks <- detect_peaks(data)

  # Expected peaks should still be detected
  expect_equal(sort(result_peaks$positions), c(3, 7, 10))
  expect_equal(sort(result_peaks$heights), c(7, 9, 10))
})

# Test detect_peaks for empty input
test_that("detect_peaks handles empty input for 1D data", {
  data <- numeric(0)

  result_peaks <- detect_peaks(data)

  expect_equal(result_peaks$positions, numeric(0))
  expect_equal(result_peaks$heights, numeric(0))
})

# Test detect_peaks function for 2D data
test_that("detect_peaks correctly identifies peaks in 2D data", {
  data_2d <- matrix(c(1, 3, 7, 1, 2, 5, 9, 3, 1, 10, 3, 2,
                      2, 6, 8, 2, 3, 4, 7, 2, 2, 9, 4, 1),
                    nrow = 2, byrow = TRUE)

  # Detect peaks for 2D data
  result_peaks <- detect_peaks(data_2d, noise_threshold = 1)

  # Check peaks in the first row (detected order: 10, 7, 3)
  expect_equal(result_peaks[[1]]$positions, c(10, 7, 3))
  expect_equal(result_peaks[[1]]$heights, c(10, 9, 7))

  # Check peaks in the second row (detected order: 10, 3, 7)
  expect_equal(result_peaks[[2]]$positions, c(10, 3, 7))
  expect_equal(result_peaks[[2]]$heights, c(9, 8, 7))
})
