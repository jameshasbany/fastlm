

## Brackets because spec_tbl_df is annoying

test_that("bootstrap_lm works with formula", {

  set.seed(24680)
  exp_ests <- readr::read_csv("bootstrap_lm-estimate.csv")[]
  exp_se <- readr::read_csv("bootstrap_lm-std_error.csv")[]
  exp_stat <- readr::read_csv("bootstrap_lm-statistics.csv")[]

  result <- bootstrap_lm(mtcars, mpg ~ hp + cyl, 100)


  expect_equal(result$estimate, exp_ests)
  expect_equal(result$std_error, exp_se)
  expect_equal(result$statistics, exp_stat)
})



test_that("bootstrap_lm works with model", {

  set.seed(24680)
  exp_ests <- readr::read_csv("bootstrap_lm-estimate.csv")[]
  exp_se <- readr::read_csv("bootstrap_lm-std_error.csv")[]
  exp_stat <- readr::read_csv("bootstrap_lm-statistics.csv")[]

  my_lm <- lm(mpg ~ hp + cyl, data = mtcars)

  result <- bootstrap_lm(mtcars, my_lm, 100)


  expect_equal(result$estimate, exp_ests)
  expect_equal(result$std_error, exp_se)
  expect_equal(result$statistics, exp_stat)
})


