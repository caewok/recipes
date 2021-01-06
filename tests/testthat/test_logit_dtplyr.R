library(testthat)
library(recipes)
library(tibble)
library(dtplyr)

context("dtplyr: Logit transformation")


n <- 20
set.seed(12)
ex_dat <- data.frame(x1 = runif(n),
                     x2 = rnorm(n))

ex_dat_dt <- lazy_dt(ex_dat)

test_that('simple logit trans', {
  rec <- recipe(~., data = ex_dat_dt) %>%
    step_logit(x1)

  rec_trained <- prep(rec, training = ex_dat_dt, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat_dt)

  exp_res <- as_tibble(ex_dat)
  exp_res$x1 <- binomial()$linkfun(exp_res$x1)
  expect_equal(rec_trans, exp_res)
})


test_that('out of bounds logit trans', {
  rec <- recipe(~., data = ex_dat_dt) %>%
    step_logit(x1, x2)

  expect_error(prep(rec, training = ex_dat_dt, verbose = FALSE))
})


test_that('printing', {
  rec <- recipe(~., data = ex_dat_dt) %>%
    step_logit(x1)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat_dt, verbose = TRUE))
})
