test_that("simple_visual_residual works", {

  response <- "mpg"
  predictor <- "hp"

  actual_result <- simple_visual_residual(mtcars, response, predictor)


  expect_equal(ggplot2::is.ggplot(actual_result), TRUE)
})
