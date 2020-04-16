test_that("check_predictors works", {

  response = c("mpg")
  explan <- c("am", "hp")
  to_try <- c("wt", "gear", "cyl")

  res <- check_predictors(mtcars, response, explan, to_try)


  correct_answer <- c("wt : Good, 0.055733.",
                      "gear : Bad, -0.000988.",
                      "cyl : Good, 0.016147.")

  expect_equal(res, correct_answer)
})
