test_that("clustered_errors_lm works", {

  expected_result <- readr::read_csv("df_for_clustered_errors.csv")[]

  actual_result <- clustered_errors_lm(mtcars, mpg ~ hp + wt, cyl)


  expect_equal(actual_result, expected_result)
})
