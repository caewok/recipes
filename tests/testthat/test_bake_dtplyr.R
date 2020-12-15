library(recipes)
library(testthat)
library(dtplyr)

mtcars_dt <- lazy_dt(mtcars)

test_that("order of columns after juice and bake",{
  car_rec <- recipe(cyl ~ ., mtcars_dt) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())
  car_preped <- prep(car_rec, training = mtcars_dt)
  expect_equal(colnames(juice(car_preped)), colnames(bake(car_preped, new_data = mtcars_dt)))
})

# dtplyr does not yet support predicates; skip test
# test_that("can use tidyselect ops in bake() and juice() column selection", {
#   car_rec <- recipe(cyl ~ ., mtcars_dt) %>%
#     step_center(all_predictors())
#
#   car_prepped <- prep(car_rec, training = mtcars_dt)
#
#   x <- bake(car_prepped, mtcars_dt, where(is.numeric) & starts_with("c") & !cyl)
#   y <- juice(car_prepped, where(is.numeric) & starts_with("c") & !cyl)
#
#   expect_named(x, "carb")
#   expect_named(y, "carb")
# })
