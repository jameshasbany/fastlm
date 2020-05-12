

test_that("bootstrap_SE_lm works", {

  set.seed(24680)
  expected_result <- readr::read_csv("bootstrap_SE_lm-expected.csv")

  response = c("mpg")
  predictors <- c("hp", "wt")
  number_of_reps <- 100

  actual_result <- bootstrap_SE_lm(mtcars, response, predictors, number_of_reps)


  expect_equal(actual_result, expected_result)
})


