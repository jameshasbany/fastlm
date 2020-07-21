test_that("corrected_errors_lm works", {

  expected_result <- readr::read_csv("df_for_corrected_errors.csv")[]
  type <- "HC1"

  actual_result <- corrected_errors_lm(mtcars, mpg ~ hp + wt + cyl, type)


  expect_equal(actual_result, expected_result)
})
