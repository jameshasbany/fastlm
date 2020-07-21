test_that("check_bias works", {



  actual_result <- check_bias(mtcars, mpg ~ cyl + hp,
                              target_predictor = hp,
                              am, gear, carb)


  expected_result <- readr::read_csv("df_for_check_bias.csv")[]

  expect_equal(actual_result, expected_result)
})


test_that("check_bias_all works", {



  actual_result <- check_bias_all(mtcars, mpg ~ cyl + hp,
                              am, gear, carb)


  expected_result <- readr::read_csv("df_for_check_bias_all.csv")[]

  expect_equal(actual_result, expected_result)
})


