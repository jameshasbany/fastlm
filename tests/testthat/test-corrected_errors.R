test_that("correctd_errors works", {

  expected_result <- readr::read_csv("df_for_corrected_errors.csv")


  response = c("mpg")
  explan <- c("am", "hp")
  type <- "HC1"

  actual_result <- corrected_errors(mtcars, response, explan, type)


  expect_equal(actual_result, expected_result)
})
