library(testthat)
library(recipes)
library(dplyr)
library(dtplyr)

context("dtplyr: Missing data checks")


set_with_na <- tibble(
  a = c(1, 2, NA),
  b = c(1, 2, NA_real_),
  d = as.integer(c(1, 2, NA_integer_)),
  e = c(1, 2, NA_character_)
)
set_with_na_dt <- lazy_dt(set_with_na)


tst <- function(...) {
  cols <- quos(...)
  recipe(set_with_na_dt) %>% check_missing(!!!cols) %>%
    prep() %>% bake(set_with_na_dt)
}

test_that("check_missing passes silently when no NA", {
  mtcars_dt <- lazy_dt(mtcars)

  no_na_rp <- recipe(mtcars_dt) %>%
    check_missing(all_numeric()) %>%
    prep()
  expect_error(bake(no_na_rp, mtcars_dt), NA)
  expect_equal(bake(no_na_rp, mtcars_dt), tibble(mtcars))
})

test_that("check_missing throws error on all types", {
  expect_error(tst(a),
              "The following columns contain missing values: `a`.")
  expect_error(tst(b),
               "The following columns contain missing values: `b`.")
  expect_error(tst(d),
               "The following columns contain missing values: `d`.")
  expect_error(tst(e),
               "The following columns contain missing values: `e`.")
})

test_that("check_missing works on multiple columns simultaneously" ,{
  expect_error(tst(a, e),
               "The following columns contain missing values: `a`, `e`.")
  expect_error(tst(everything()),
               paste0("The following columns contain missing values: ",
                      "`a`, `b`, `d`, `e`."))
})

test_that("check_missing on a new set", {
  no_na <- tibble(a = 1:3)
  na    <- tibble(a = c(1, NA))
  no_na_dt <- lazy_dt(no_na)
  na_dt <- lazy_dt(na)

  rp    <- recipe(no_na_dt) %>% check_missing(a) %>% prep(no_na_dt)
  expect_error(bake(rp, na_dt),
               "The following columns contain missing values: `a`.")
})
