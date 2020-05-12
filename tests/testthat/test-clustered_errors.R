test_that("clustered_errors works", {

  expected_result <- readr::read_csv("df_for_clustered_errors.csv")


  response = c("mpg")
  explan <- c("am", "hp")
  cluster_predictor <- mtcars$cyl

  actual_result <- clustered_errors(mtcars, response, explan, cluster_predictor)


  expect_equal(actual_result, expected_result)
})
