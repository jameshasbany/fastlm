test_that("check_bias works", {

  response = c("mpg")
  predictor_to_check <- c("hp")
  omitted_variables <- c("wt")
  other_predictors <- c("cyl")

  actual_result <- check_bias(mtcars, response, predictor_to_check, other_predictors, omitted_variables)


  expected_result <- c("wt: OVB = -0.001084.")

  expect_equal(actual_result, expected_result)
})


