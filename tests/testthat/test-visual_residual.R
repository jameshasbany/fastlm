test_that("visual_residual works", {

  response = "mpg"
  predictor <- c("am", "hp")

  actual_result <- visual_residual(mtcars, response, predictor)


  expect_equal(ggplot2::is.ggplot(actual_result), TRUE)
})
