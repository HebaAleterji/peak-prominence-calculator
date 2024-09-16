test_that("detect_peaks correctly identifies peaks in 2D data", {
  # Test data: a matrix with different peaks in rows
  data <- matrix(c(1, 3, 12, 6, 7, 10, 12, 5, 3,
                   2, 8, 10, 4, 5, 11, 14, 6, 1), nrow = 2, byrow = TRUE)

  # Detect peaks row-wise
  result_list <- detect_peaks(data, min_peak_height = 5, apply_across = "row")

  # Check if the function detects peaks in both rows correctly
  expect_equal(result_list[[1]]$Position, c(3, 6))
  expect_equal(result_list[[1]]$Height, c(12, 12))

  expect_equal(result_list[[2]]$Position, c(2, 6, 7))
  expect_equal(result_list[[2]]$Height, c(8, 11, 14))
})
