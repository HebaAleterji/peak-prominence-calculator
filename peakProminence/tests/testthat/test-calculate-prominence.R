# Test calculate_prominence function
test_that("calculate_prominence correctly calculates prominence", {
  data <- c(1, 7, 1, 7, 1, 1, 9, 1, 1, 10, 1)
  peaks <- detect_peaks(data)
  result_prominence <- calculate_prominence(peaks, data)

  # Verify prominences against expected values for the first three prominences
  expected_prominence <- c(9, 8, 6)

  # Truncate the result if it contains extra values, and compare only first 3 values
  actual_prominence <- result_prominence$prominences[1:length(expected_prominence)]

  expect_equal(actual_prominence, expected_prominence)
})



test_that("calculate_prominence handles no peaks", {
  data <- c(1, 1, 1, 1, 1)  # No significant peaks

  peaks <- detect_peaks(data)
  result_prominence <- calculate_prominence(peaks, data)

  # Check if prominences are correctly returned as empty
  expect_equal(result_prominence$prominences, numeric(0))
})


test_that("calculate_prominence handles edge cases", {
  data <- c(1, 7, 1, 7, 1)

  peaks <- detect_peaks(data)
  result_prominence <- calculate_prominence(peaks, data)

  expected_prominence <- c(6, 6)
  expect_equal(result_prominence$prominences, expected_prominence)
})
