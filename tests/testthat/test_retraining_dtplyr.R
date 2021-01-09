library(testthat)
library(recipes)
library(dtplyr)

context("dtplyr: Testing retraining")

library(modeldata)
data(biomass)

biomass_dt <- lazy_dt(biomass)

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_dt)

test_that('training in stages', {

  whole_recipe <- rec %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) %>%
    step_rm(sulfur) %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen)

  at_same_time <- prep(whole_recipe, training = biomass_dt)

  ## not train in stages
  center_first <- rec %>%
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur)
  center_first_trained <-
    prep(center_first, training = biomass_dt)

  no_sulfur <- center_first_trained %>%
    step_rm(sulfur)
  no_sulfur_trained <-
    prep(no_sulfur)

  scale_last <- no_sulfur_trained %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen)
  sequentially <- prep(scale_last)


  in_stages <- center_first_trained  %>%
    step_rm(sulfur) %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen)
  in_stages_trained <-
    prep(in_stages)
  in_stages_retrained <-
    prep(in_stages, training = biomass, fresh = TRUE)

  # check baked values

  expect_equal(
    bake(at_same_time, head(biomass_dt)),
    bake(sequentially, head(biomass_dt))
  )
  expect_equal(
    bake(at_same_time, head(biomass_dt)),
    bake(in_stages_trained, head(biomass_dt))
  )
  expect_equal(
    bake(at_same_time, head(biomass_dt)),
    bake(in_stages_retrained, head(biomass_dt))
  )

  # variable lists
  expect_equal(
    summary(at_same_time),
    summary(sequentially)
  )
  expect_equal(
    summary(at_same_time),
    summary(in_stages_trained)
  )
  expect_equal(
    summary(at_same_time),
    summary(in_stages_retrained)
  )

  expect_error(
    rec %>%
      step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) %>%
      prep(training = biomass_dt) %>%
      step_rm(sulfur) %>%
      prep(training = biomass_dt),
    regexp = NA
  )

})
