library(testthat)
library(recipes)
library(dtplyr)

context("dtplyr: correlation filter")


n <- 100
set.seed(424)
dat <- matrix(rnorm(n*5), ncol =  5)
dat <- as.data.frame(dat)
dat$duplicate <- dat$V1
dat$V6 <- -dat$V2 + runif(n)*.2

dat_dt <- lazy_dt(dat)

test_that('high filter', {
  set.seed(1)
  rec <- recipe(~ ., data = dat_dt)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = .5)

  expect_warning(filtering_trained <- prep(filtering, training = dat_dt, verbose = FALSE))

  removed <- c("V6", "V1")

  expect_equal(filtering_trained$steps[[1]]$removals, removed)
})

test_that('low filter', {
  rec <- recipe(~ ., data = dat_dt)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = 1)

  expect_warning(filtering_trained <- prep(filtering, training = dat_dt, verbose = FALSE))

  expect_equal(filtering_trained$steps[[1]]$removals, numeric(0))
})

test_that('many missing values', {
  dat2 <- dat
  dat2$V4 <- NA_real_
  dat2_dt <- lazy_dt(dat2)
  rec <- recipe(~ ., data = dat2_dt)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = .25)

  filtering_trained <-
    expect_warning(
      prep(filtering, training = dat2_dt, verbose = FALSE),
      "1 columns were excluded from the filter"
    )

  expect_equal(filtering_trained$steps[[1]]$removals, paste0("V", 1:2))
})

test_that('occasional missing values', {
  dat3 <- dat
  dat3$V1[1] <- NA_real_
  dat3$V4[10] <- NA_real_
  dat3_dt <- lazy_dt(dat3)
  rec <- recipe(~ ., data = dat3)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = .25, use = "everything")

  filtering_trained <-
    expect_warning(
      prep(filtering, training = dat3_dt, verbose = FALSE),
      "Some columns were excluded from the filter"
    )

  expect_equal(filtering_trained$steps[[1]]$removals, "V2")
})


test_that('printing', {
  set.seed(1)
  rec <- recipe(~ ., data = dat_dt)
  filtering <- rec %>%
    step_corr(all_predictors(), threshold = .5)
  expect_output(print(filtering))
  expect_output(expect_warning(prep(filtering, training = dat_dt, verbose = TRUE)))
})


test_that('tunable', {
  iris_dt <- lazy_dt(iris)

  rec <-
    recipe(~ ., data = iris_dt) %>%
    step_corr(all_predictors())
  rec_param <- tunable.step_corr(rec$steps[[1]])
  expect_equal(rec_param$name, c("threshold"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})
