library(testthat)
library(recipes)
library(dtplyr)

context("dtplyr: class distances")

# Note: some tests convert to data frame prior to testing
# https://github.com/tidyverse/dplyr/issues/2751

eps <- if (capabilities("long.double"))
  sqrt(.Machine$double.eps) else
  0.1

iris_dt <- lazy_dt(iris)

test_that("defaults", {
  rec <- recipe(Species ~ ., data = iris_dt) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE, id = "")
  expect_warning(trained <- prep(rec, training = iris_dt, verbose = FALSE))
  dists <- bake(trained, new_data = iris_dt)
  dists <- dists[, grepl("classdist", names(dists))]
  dists <- as.data.frame(dists)

  split_up <- split(iris[, 1:4], iris$Species)
  mahalanobis2 <- function(x, y)
    mahalanobis(y, center = colMeans(x), cov = cov(x))

  exp_res <- lapply(split_up, mahalanobis2, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for(i in 1:ncol(exp_res))
    expect_equal(dists[, i], exp_res[, i])

  tidy_exp_un <- tibble(
    terms = "all_predictors()",
    value = NA_real_,
    class = NA_character_,
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(rec, number = 1))
  means <- lapply(split_up, colMeans)
  means <- unlist(unname(means))

  tidy_exp_tr <- tibble(
    terms = names(means),
    value = unname(means),
    class = rep(names(split_up), each = 4),
    id = ""
  )
  expect_equal(
    as.data.frame(tidy_exp_tr),
    as.data.frame(tidy(trained, number = 1)),
    tolerance = eps
  )
})

test_that("alt args", {
  rec <- recipe(Species ~ ., data = iris_dt) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE, mean_func = median)
  expect_warning(trained <- prep(rec, training = iris_dt, verbose = FALSE))
  dists <- bake(trained, new_data = iris_dt)
  dists <- dists[, grepl("classdist", names(dists))]
  dists <- as.data.frame(dists)

  split_up <- split(iris[, 1:4], iris$Species)
  mahalanobis2 <- function(x, y)
    mahalanobis(y, center = apply(x, 2, median), cov = cov(x))

  exp_res <- lapply(split_up, mahalanobis2, y = iris[, 1:4])
  exp_res <- as.data.frame(exp_res)

  for(i in 1:ncol(exp_res))
    expect_equal(dists[, i], exp_res[, i])
})

test_that('printing', {
  rec <- recipe(Species ~ ., data = iris_dt) %>%
    step_classdist(all_predictors(), class = "Species", log = FALSE)
  expect_output(print(rec))
  expect_output(expect_warning(prep(rec, training = iris_dt, verbose = TRUE)))
})

test_that('prefix', {
  rec <- recipe(Species ~ ., data = iris_dt) %>%
    step_classdist(all_predictors(), class = "Species",
                   log = FALSE, prefix = "centroid_")
  expect_warning(trained <- prep(rec, training = iris_dt, verbose = FALSE))
  dists <- bake(trained, new_data = iris_dt)
  expect_false(any(grepl("classdist_", names(dists))))
  expect_true(any(grepl("centroid_", names(dists))))
})
