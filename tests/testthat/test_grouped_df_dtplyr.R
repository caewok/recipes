library(tibble)
library(dplyr)
library(dtplyr)

context("dtplyr: Grouped data frame tests")


n <- 20
ex_dat <-
  data.frame(x1 = seq(0, 1, length = n), x2 = rep(1:5, 4)) %>%
  group_by(x1)

ex_dat_dt <- lazy_dt(ex_dat)

# Test fixes for Issue #125
test_that("grouped data frames work", {
  n <- 20
  rec <- recipe(~., data = ex_dat_dt) %>%
    step_poly(x2, degree = 2)

  rec_trained <- prep(rec, training = ex_dat_dt, verbose = FALSE)
  rec_trans <- bake(rec_trained, new_data = ex_dat_dt)
  expect_equal(names(rec_trans), c("x1", "x2_poly_1", "x2_poly_2"))

})


