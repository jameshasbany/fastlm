test_that("check_predictors works", {



  actual_result <- check_predictors(mtcars, hp ~ cyl + log(wt), mpg, am, disp)


  expected_result <- readr::read_csv("df_for_check_predictors.csv")[]

  expect_equal(actual_result, expected_result)
})
