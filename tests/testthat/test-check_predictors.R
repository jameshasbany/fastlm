test_that("check_predictors works", {

  response = c("mpg")
  explan <- c("am", "hp")
  to_try <- c("wt", "gear", "cyl")

  actual_result <- check_predictors(mtcars, response, explan, to_try)


  expected_result <- c("wt: Good, 0.055733.",
                       "gear: Bad, -0.000988.",
                       "cyl: Good, 0.016147." )

  expect_equal(actual_result, expected_result)
})
