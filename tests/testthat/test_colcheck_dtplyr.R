library(testthat)
library(recipes)
library(dplyr)
library(dtplyr)

context("dtplyr: column checking")


mtcars_dt <- lazy_dt(mtcars)
rp1 <- recipe(mtcars_dt, cyl ~ .)
rp2 <- recipe(mtcars_dt, cyl ~ mpg + drat)

test_that("check_col works in the prep stage", {
  expect_error(rp1 %>% check_cols(everything()) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(everything()) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep(), NA)
  expect_error(rp2 %>% check_cols(cyl, mpg) %>% prep(), NA)
})


test_that("check_col works in the bake stage", {
  expect_error(rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars_dt),
               NA)
  expect_equal(rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars_dt),
               tibble(mtcars[ ,c(1, 3:11, 2)]))
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>% bake(mtcars_dt), NA)
  expect_equal(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>% bake(mtcars_dt),
               tibble(mtcars[ ,c(1, 5, 2)]))

  # add a compute to avoid a weird "." function not found when running Test Package.
  expect_error(rp1 %>% check_cols(everything()) %>% prep %>% bake(mtcars_dt %>% compute() %>% dplyr::select(-mpg)),
               "The following cols are missing from `new_data`: `mpg`.")
  expect_error(rp2 %>% check_cols(cyl, mpg, drat) %>% prep %>%
                 bake(mtcars_dt %>% compute() %>% dplyr::select(cyl, disp, hp, drat)),
               "The following cols are missing from `new_data`: `mpg`.")
})
