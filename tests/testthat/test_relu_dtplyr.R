library(testthat)
library(recipes)
library(dtplyr)

context("dtplyr: step_relu")

df <- tibble(val1 = -10:10, val2 = factor(LETTERS[1:21]))
df_dt <- lazy_dt(df)

test_that('default relu settings', {
  baked <- recipe(~ ., data = df_dt) %>%
    step_relu(val1) %>%
    prep(df_dt, verbose = FALSE) %>%
    bake(df_dt)

  expected_baked <- df %>%
    dplyr::mutate(right_relu_val1 = pmax(rep(0, length(val1)), val1))

  expect_equal(baked, expected_baked)
})


test_that('shifted and reversed relu', {
  baked <- recipe(~ ., data = df_dt) %>%
    step_relu(val1, shift = 5, reverse = TRUE) %>%
    prep(df_dt, verbose = FALSE) %>%
    bake(df_dt)

  expected_baked <- df %>%
    dplyr::mutate(left_relu_val1 = pmax(rep(0, length(val1)), -(val1 - 5)))

  expect_equal(baked, expected_baked)
})


test_that('reversed softplus', {
  baked <- recipe(~ ., data = df_dt) %>%
    step_relu(val1, smooth = TRUE, reverse = TRUE) %>%
    prep(df_dt, verbose = FALSE) %>%
    bake(df_dt)

  expected_baked <- df %>%
    dplyr::mutate(left_relu_val1 = log1p(exp(-val1)))

  expect_equal(baked, expected_baked)
})


test_that('shifted and prefixed softplus', {
  baked <- recipe(~ ., data = df_dt) %>%
    step_relu(val1, shift = 5, smooth = TRUE, prefix = "sp_") %>%
    prep(df_dt, verbose = FALSE) %>%
    bake(df_dt)

  expected_baked <- df %>%
    dplyr::mutate(sp_val1 = log1p(exp(val1 - 5)))

  expect_equal(baked, expected_baked)
})

test_that('works with all_predictors() selector', {
  iris_dt <- lazy_dt(iris)

  expect_silent({
    rec <- recipe(Species ~ ., data = iris_dt) %>%
    step_relu(all_predictors())
  })

  expect_silent(prepped_rec <- prep(rec, iris_dt))
})


test_that('input checking', {
   expect_error(
    recipe(~ ., data = df_dt) %>%
      step_relu(val1, shift = TRUE) %>%  # wrong argument type to shift
      prep(df_dt, verbose = FALSE),
    "numeric"
  )

  expect_error(
    recipe(~ ., data = df_dt) %>%
      step_relu(val1, reverse = 3) %>%  # wrong argument type to reverse
      prep(df_dt, verbose = FALSE),
    "logical"
  )

  expect_error(
    recipe(~ ., data = df_dt) %>%
      step_relu(val1, smooth = "cat") %>%  # wrong argument type to smooth
      prep(df_dt, verbose = FALSE),
    "logical"
  )

  expect_error(
    recipe(~ ., data = df_dt) %>%
      step_relu(val2) %>%  # apply to non-numeric column
      prep(df_dt, verbose = FALSE),
    "numeric"
  )
})

test_that('prints something', {
  rec <- recipe(~ ., data = df_dt) %>%
    step_relu(val1)
  expect_output(print(rec))
  expect_output(prep(rec, training = df_dt, verbose = TRUE))
})

rm(df)
