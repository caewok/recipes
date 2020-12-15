library(testthat)
library(recipes)
library(dtplyr)

context("dtplyr: Regex pattern matching indicators")

library(modeldata)
data(covers)
covers$rows <- 1:nrow(covers)
covers$ch_rows <- paste(1:nrow(covers))

covers_dt <- lazy_dt(covers)

rec <- recipe(~ description + rows + ch_rows, covers_dt)

test_that('default options', {
  rec1 <- rec %>%
    step_regex(description, pattern = "(rock|stony)") %>%
    step_regex(description, result = "all ones")
  rec1 <- prep(rec1, training = covers_dt)
  res1 <- bake(rec1, new_data = covers_dt)
  expect_equal(res1$X.rock.stony.,
               as.numeric(grepl("(rock|stony)", covers$description)))
  expect_equal(res1$`all ones`, rep(1, nrow(covers)))
})


test_that('nondefault options', {
  rec2 <- rec %>%
    step_regex(description, pattern = "(rock|stony)",
               result = "rocks",
               options = list(fixed = TRUE))
  rec2 <- prep(rec2, training = covers_dt)
  res2 <- bake(rec2, new_data = covers_dt)
  expect_equal(res2$rocks, rep(0, nrow(covers)))
})


test_that('bad selector(s)', {
  expect_error(rec %>% step_regex(description, rows, pattern = "(rock|stony)"))
  rec3 <- rec %>% step_regex(starts_with("b"), pattern = "(rock|stony)")
  expect_error(prep(rec3, training = covers_dt))
  rec4 <- rec %>% step_regex(rows, pattern = "(rock|stony)")
  expect_error(prep(rec4, training = covers_dt))
})


test_that('printing', {
  rec1 <- rec %>%
    step_regex(description, pattern = "(rock|stony)")
  expect_output(print(rec1))
  expect_output(prep(rec1, training = covers_dt, verbose = TRUE))
})
