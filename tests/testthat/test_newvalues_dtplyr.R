library(dtplyr)
library(modeldata)
context("dtplyr: Check new values")


x    <- rep(letters[1:3], 2)
x_na <- c(rep(letters[1:3], 2), NA)
allowed_values <- letters[1:3]

test_that("new_values_func passes when no new values", {
  expect_error(new_values_func(x, allowed_values), NA)
})

test_that("new_values_func breaks when x contains new values", {
  expect_error(new_values_func(x, allowed_values[-3], colname = "MacGyver"),
               "MacGyver contains the new value(s): c", fixed = TRUE)
})

test_that("new_values_func correctly prints multiple new values", {
  expect_error(new_values_func(x, allowed_values[-c(2:3)], colname = "MacGyver"),
               "MacGyver contains the new value(s): b,c", fixed = TRUE)
})

test_that("new_values_func by default ignores NA", {
  expect_error(new_values_func(x_na, allowed_values), NA)
})

test_that("new_values_func breaks when NA is new value and ignore_NA is FALSE", {
  expect_error(new_values_func(x_na, allowed_values,
                               ignore_NA = FALSE, colname = "MacGyver"),
               "MacGyver contains the new value(s): NA", fixed = TRUE)
})

test_that("new_values_func correctly prints multiple new values with NA", {
  expect_error(new_values_func(x_na, allowed_values[-3],
                               ignore_NA = FALSE, colname = "MacGyver"),
               "MacGyver contains the new value(s): c,NA", fixed = TRUE)
})

test_that("new_values_func correctly prints only non na-values when also NA as new value and ignore_NA is TRUE", {
  expect_error(new_values_func(x_na, allowed_values[-3],
                               ignore_NA = TRUE, colname = "MacGyver"),
               "MacGyver contains the new value(s): c", fixed = TRUE)
})

test_that("check_new_values does nothing when no new values", {
  data(credit_data)
  credit_data_dt <- lazy_dt(credit_data)

  expect_error(
    x <- recipe(credit_data_dt) %>% check_new_values(Home) %>%
      prep() %>% bake(credit_data_dt),
    NA
  )
  expect_equal(x, as_tibble(credit_data))
})

test_that("check_new_values breaks with new values", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:5])
  x1_dt <- lazy_dt(x1)
  x2_dt <- lazy_dt(x2)

  expect_error(
    recipe(x1_dt) %>% check_new_values(a) %>%
      prep() %>% bake(x2_dt %>% compute() %>% dplyr::slice(1:4)),
    "a contains the new value(s): d", fixed = TRUE
  )

  expect_error(
    recipe(x1_dt) %>% check_new_values(a) %>%
      prep() %>% bake(x2_dt),
    "a contains the new value(s): d,e", fixed = TRUE
  )
})

test_that("check_new_values ignores NA by default", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:4] %>% c(NA))
  x1_dt <- lazy_dt(x1)
  x2_dt <- lazy_dt(x2)

  expect_error(
    recipe(x1_dt) %>% check_new_values(a) %>%
      prep() %>% bake(x2_dt %>% compute() %>% dplyr::slice(-4)),
    NA
  )

  expect_error(
    recipe(x1_dt) %>% check_new_values(a) %>%
      prep() %>% bake(x2_dt),
    "a contains the new value(s): d", fixed = TRUE
  )
})

test_that("check_new_values not ignoring NA argument", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:4] %>% c(NA))
  x1_dt <- lazy_dt(x1)
  x2_dt <- lazy_dt(x2)

  expect_error(
    recipe(x1_dt) %>% check_new_values(a, ignore_NA = FALSE) %>%
      prep() %>% bake(x2_dt %>% compute() %>% dplyr::slice(-4)),
    "a contains the new value(s): NA", fixed = TRUE
  )

  expect_error(
    recipe(x1_dt) %>% check_new_values(a, ignore_NA = FALSE) %>%
      prep() %>% bake(x2_dt),
    "a contains the new value(s): d,NA", fixed = TRUE
  )
})

check_new_values_data_type_unit_tests <- function(x1, x2, saf = TRUE) {
  x1_dt <- lazy_dt(x1)
  x2_dt <- lazy_dt(x2)

  expect_error(
    res <- recipe(x1_dt) %>% check_new_values(a) %>%
      prep(strings_as_factors = saf) %>% bake(x1_dt),
    NA
  )

  expect_equal(res, x1)

  error_msg <- paste( "a contains the new value(s):", pull(x2[3,], a))
  expect_error(
    recipe(x1_dt) %>% check_new_values(a) %>%
      prep() %>% bake(x2_dt),
    error_msg, fixed = TRUE
  )
}

test_that("check_new_values works on doubles", {
  x1 <- tibble(a = c(1.1, 1.2))
  x2 <- tibble(a = c(1.1, 1.2, 1.3))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on integers", {
  x1 <- tibble(a = c(1L, 2L))
  x2 <- tibble(a = c(1L, 2L, 3L))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on factors", {
  x1 <- tibble(a = factor(letters[1:2]))
  x2 <- tibble(a = factor(letters[1:3]))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on characters", {
  x1 <- tibble(a = letters[1:2])
  x2 <- tibble(a = letters[1:3])
  check_new_values_data_type_unit_tests(x1, x2, saf = FALSE)
})

test_that("check_new_values works on logicals", {
  x1 <- tibble(a = c(TRUE, TRUE))
  x2 <- tibble(a = c(TRUE, TRUE, FALSE))
  check_new_values_data_type_unit_tests(x1, x2)
})
