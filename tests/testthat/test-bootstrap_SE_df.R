

test_that("bootstrap_SE_df works", {
  set.seed(24680)
  response = c("mpg")
  predictors <- c("hp", "wt")
  number_of_reps <- 100

  actual_result <- round(bootstrap_SE_df(mtcars, response, predictors, number_of_reps), 3)


  expected_result <- round(readr::read_csv("bootstrap_SE_df-expected.csv"), 3)

  expect_equal(actual_result, expected_result)
})


