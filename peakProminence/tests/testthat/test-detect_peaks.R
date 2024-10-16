# Test detect_peaks function for 1D data
test_that("detect_peaks correctly identifies peaks in 1D data", {
  data <- c(1, 3, 7, 1, 2, 5, 9, 3, 1, 10, 3)

  # Detect peaks with a noise threshold of 1
  result_peaks <- detect_peaks(data, noise_threshold = 1)

  # Sort positions and heights before comparison
  expect_equal(sort(result_peaks$positions), sort(c(3, 7, 10)))
  expect_equal(sort(result_peaks$heights), sort(c(7, 9, 10)))
})

# Test detect_peaks with adaptive noise threshold
test_that("detect_peaks adapts noise_threshold based on data", {
  data <- c(1, 3, 7, 1, 2, 5, 9, 3, 1, 10, 3)

  # Detect peaks with automatically calculated noise threshold
  result_peaks <- detect_peaks(data)

  # Sort positions and heights before comparison
  expect_equal(sort(result_peaks$positions), sort(c(3, 7, 10)))
  expect_equal(sort(result_peaks$heights), sort(c(7, 9, 10)))
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

  # Sort positions and heights before comparison for the first row
  expect_equal(sort(result_peaks[[1]]$positions), sort(c(10, 7, 3)))
  expect_equal(sort(result_peaks[[1]]$heights), sort(c(10, 9, 7)))

  # Sort positions and heights before comparison for the second row
  expect_equal(sort(result_peaks[[2]]$positions), sort(c(10, 3, 7)))
  expect_equal(sort(result_peaks[[2]]$heights), sort(c(9, 8, 7)))
})
