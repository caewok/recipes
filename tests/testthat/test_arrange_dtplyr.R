library(testthat)
library(recipes)
library(dplyr)
library(dtplyr)

# ------------------------------------------------------------------------------

context("dplyr arrange steps")

# ------------------------------------------------------------------------------

iris_dt <- lazy_dt(iris)
iris_rec <- recipe( ~ ., data = iris_dt)

# ------------------------------------------------------------------------------

test_that('basic usage', {
  rec <-
    iris_rec %>%
    step_arrange(desc(Sepal.Length), 1/Petal.Length)

  prepped <- prep(rec, training = iris_dt %>% slice(1:75))

  dplyr_train <-
    iris_dt %>%
    slice(1:75) %>%
    dplyr::arrange(desc(Sepal.Length), 1/Petal.Length)

  rec_train <- juice(prepped)
  expect_equal(dplyr_train %>% as_tibble(), rec_train %>% as_tibble())

  dplyr_test <-
    iris_dt %>%
    slice(76:150) %>%
    dplyr::arrange(desc(Sepal.Length), 1/Petal.Length)
  rec_test <- bake(prepped, iris_dt %>% slice(76:150))
  expect_equal(dplyr_test %>% as_tibble(), rec_test %>% as_tibble())
})

test_that('quasiquotation', {
  sort_vars <- c("Sepal.Length", "Petal.Length")
  sort_vars <- syms(sort_vars)
  rec_1 <-
    iris_rec %>%
    step_arrange(!!!sort_vars)

  prepped_1 <- prep(rec_1, training = iris_dt %>% slice(1:75))

  dplyr_train <-
    iris_dt %>%
    slice(1:75) %>%
    arrange(Sepal.Length, Petal.Length)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train %>% as_tibble(), rec_1_train %>% as_tibble())

})

test_that('no input', {
  no_inputs <-
    iris_rec %>%
    step_arrange() %>%
    prep(training = iris_dt) %>%
    juice(composition = "data.frame")
  expect_equal(no_inputs, iris)
})

test_that('printing', {
  rec <- iris_rec %>% step_arrange(Sepal.Length)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris_dt, verbose = TRUE))
})

