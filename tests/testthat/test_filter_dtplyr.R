library(testthat)
library(recipes)
library(dplyr)
library(dtplyr)

# ------------------------------------------------------------------------------

context("dtplyr: dplyr filter steps")

# ------------------------------------------------------------------------------
iris_dt <- lazy_dt(iris) %>% compute() # compute required to fix a weird error of not finding functions when running Test Package.

iris_rec <- recipe( ~ ., data = iris_dt)

# ------------------------------------------------------------------------------

test_that('basic usage - skip = FALSE', {
  rec <-
    iris_rec %>%
    step_filter(Sepal.Length > 4.5, Species == "setosa", skip = FALSE)

  prepped <- prep(rec, training = iris_dt %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")
  dplyr_test <- dplyr_test[, names(rec_train)]

  rec_test <- bake(prepped, iris_dt %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})


test_that('skip = FALSE', {
  rec <-
    iris_rec %>%
    step_filter(Sepal.Length > 4.5, Species == "setosa", skip = FALSE)

  prepped <- prep(rec, training = iris_dt %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")

  rec_train <- juice(prepped)
  expect_equal(dplyr_train, rec_train)

  dplyr_test <-
    iris %>%
    as_tibble() %>%
    slice(76:150) %>%
    dplyr::filter(Sepal.Length > 4.5, Species == "setosa")
  rec_test <- bake(prepped, iris_dt %>% slice(76:150))
  expect_equal(dplyr_test, rec_test)
})

test_that('quasiquotation', {
  values <- c("versicolor", "virginica")
  rec_1 <-
    iris_rec %>%
    step_filter(Sepal.Length > 4.5, Species %in% !!values) # the !! is just for running within test_that

  prepped_1 <- prep(rec_1, training = iris_dt %>% slice(1:75))

  dplyr_train <-
    iris %>%
    as_tibble() %>%
    slice(1:75) %>%
    filter(Sepal.Length > 4.5, Species  %in% values)

  rec_1_train <- juice(prepped_1)
  expect_equal(dplyr_train, rec_1_train)

  rec_2 <-
    iris_rec %>%
    step_filter(Sepal.Length > 4.5, Species  %in% !!values)

  prepped_2 <- prep(rec_2, training = iris_dt %>% slice(1:75))

  # rm(values)
  # expect_error(prep(rec_1, training = iris_dt %>% slice(1:75)))
  # expect_error(
  #   prepped_2 <- prep(rec_2, training = iris_dt %>% slice(1:75)),
  #   regexp = NA
  # )
  # rec_2_train <- juice(prepped_2)
  # expect_equal(dplyr_train, rec_2_train)
})

test_that('no input', {
  no_inputs <-
    iris_rec %>%
    step_filter() %>%
    prep(training = iris_dt) %>%
    juice(composition = "data.frame")
  expect_equal(no_inputs, iris)
})


test_that('printing', {
  rec <- iris_rec %>% step_filter(Sepal.Length > 4.5)
  expect_output(print(rec))
  expect_output(prep(rec, training = iris_dt, verbose = TRUE))
})

