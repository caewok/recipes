library(recipes)
library(dplyr)
library(testthat)
library(dtplyr)

context("Checking factors in new data versus original")

# ----------------------------------------------------------------

library(modeldata)
data("okc")

okc_chr <-
  okc %>%
  mutate(Class = as.character(Class))

okc_fac <-
  okc %>%
  mutate(diet = as.factor(diet))

okc_all_fac <-
  okc_fac %>%
  mutate(location = as.factor(location))

# ----------------------------------------------------------------

test_that('factors all the way down', {
  tr <-
    okc_all_fac %>%
    slice(1:500)

  te <-
    okc_all_fac %>%
    slice(501:1000)

  tr_dt <- lazy_dt(tr) %>% compute()
  te_dt <- lazy_dt(te) %>% compute()

  rec <-
    recipe(Class ~ ., data = tr_dt) %>%
    prep(training = tr_dt)

  check_nominal_type(te_dt, rec$orig_lvls)
  expect_silent(check_nominal_type(te_dt, rec$orig_lvls))
})


test_that('factors all the way down with skipping', {
  tr <-
    okc_all_fac %>%
    slice(1:500)

  te <-
    okc_all_fac %>%
    slice(501:1000) %>%
    select(-Class)

  tr_dt <- lazy_dt(tr) %>% compute()
  te_dt <- lazy_dt(te) %>% compute()

  rec <-
    recipe(Class ~ ., data = tr_dt) %>%
    prep(training = tr_dt)

  expect_silent(check_nominal_type(te_dt, rec$orig_lvls))
})

# ----------------------------------------------------------------

test_that('mixed nominal data', {
  tr <-
    okc_fac %>%
    slice(1:500)
  te <-
    okc_fac %>%
    slice(501:1000)

  tr_dt <- lazy_dt(tr) %>% compute()
  te_dt <- lazy_dt(te) %>% compute()

  rec <-
    recipe(Class ~ ., data = tr_dt) %>%
    prep(training = tr_dt)

  expect_silent(check_nominal_type(te_dt, rec$orig_lvls))
})


test_that('mixed nominal data with skipping', {
  tr <-
    okc_fac %>%
    slice(1:500)
  te <-
    okc_fac %>%
    slice(501:1000) %>%
    select(-Class)

  tr_dt <- lazy_dt(tr) %>% compute()
  te_dt <- lazy_dt(te) %>% compute()

  rec <-
    recipe(Class ~ ., data = tr_dt) %>%
    prep(training = tr_dt)

  expect_silent(check_nominal_type(te_dt, rec$orig_lvls))
})


# ----------------------------------------------------------------

test_that('no factors', {
  tr <-
    okc_chr %>%
    slice(1:500)
  te <-
    okc_chr %>%
    slice(501:1000)

  tr_dt <- lazy_dt(tr) %>% compute()
  te_dt <- lazy_dt(te) %>% compute()

  rec <-
    recipe(Class ~ ., data = tr_dt) %>%
    prep(training = tr_dt)

  expect_silent(check_nominal_type(te_dt, rec$orig_lvls))
})


test_that('no factors with skipping', {
  tr <-
    okc_chr %>%
    slice(1:500)
  te <-
    okc_chr %>%
    slice(501:1000) %>%
    select(-Class)

  tr_dt <- lazy_dt(tr)
  te_dt <- lazy_dt(te)

  rec <-
    recipe(Class ~ ., data = tr_dt) %>%
    prep(training = tr_dt)

  expect_silent(check_nominal_type(te_dt, rec$orig_lvls))
})


# ----------------------------------------------------------------

test_that('missing factors', {
  tr <-
    okc_fac %>%
    slice(1:500)
  te <-
    okc_chr %>%
    slice(501:1000)

  tr_dt <- lazy_dt(tr) %>% compute()
  te_dt <- lazy_dt(te) %>% compute()

  rec <-
    recipe(Class ~ ., data = tr_dt) %>%
    prep(training = tr_dt)

  expect_warning(check_nominal_type(te_dt, rec$orig_lvls))
})

test_that('missing factors with skipping', {
  tr <-
    okc_fac %>%
    slice(1:500)
  te <-
    okc_chr %>%
    slice(501:1000) %>%
    select(-Class)

  tr_dt <- lazy_dt(tr) %>% compute()
  te_dt <- lazy_dt(te) %>% compute()

  rec <-
    recipe(Class ~ ., data = tr_dt) %>%
    prep(training = tr_dt)

  expect_warning(check_nominal_type(te_dt, rec$orig_lvls))
})
