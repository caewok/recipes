library(testthat)
library(recipes)
library(lubridate)
library(dtplyr)

context("dtplyr: Holiday features")


exp_dates <- data.frame(date  = ymd(c("2017-12-25", "2017-05-29", "2017-04-16")),
                        holiday = c("ChristmasDay", "USMemorialDay", "Easter"),
                        stringsAsFactors = FALSE)
test_data <- data.frame(day  = ymd("2017-01-01") + days(0:364),
                        stringsAsFactors = FALSE)

test_data_dt <- lazy_dt(test_data)

test_that('Date class', {
  holiday_rec <- recipe(~ day, test_data_dt) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  holiday_rec <- prep(holiday_rec, training = test_data_dt)
  holiday_ind <- bake(holiday_rec, test_data_dt)

  all.equal(holiday_ind$day[holiday_ind$day_USMemorialDay == 1],
            exp_dates$date[exp_dates$holiday == "USMemorialDay"])

  expect_equal(holiday_ind$day[holiday_ind$day_USMemorialDay == 1],
               exp_dates$date[exp_dates$holiday == "USMemorialDay"])
  expect_equal(holiday_ind$day[holiday_ind$day_ChristmasDay == 1],
               exp_dates$date[exp_dates$holiday == "ChristmasDay"])
  expect_equal(holiday_ind$day[holiday_ind$day_Easter == 1],
               exp_dates$date[exp_dates$holiday == "Easter"])
})


test_that('POSIXct class', {
  test_data_dt <- test_data_dt %>%
    compute() %>%
    dplyr::mutate(day = as.POSIXct(day))


  exp_dates$date <- as.POSIXct(exp_dates$date)

  holiday_rec <- recipe(~ day, test_data_dt) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)

  holiday_rec <- prep(holiday_rec, training = test_data_dt)
  holiday_ind <- bake(holiday_rec, test_data_dt)

  all.equal(holiday_ind$day[holiday_ind$day_USMemorialDay == 1],
            exp_dates$date[exp_dates$holiday == "USMemorialDay"])

  expect_equal(holiday_ind$day[holiday_ind$day_USMemorialDay == 1],
               exp_dates$date[exp_dates$holiday == "USMemorialDay"])
  expect_equal(holiday_ind$day[holiday_ind$day_ChristmasDay == 1],
               exp_dates$date[exp_dates$holiday == "ChristmasDay"])
  expect_equal(holiday_ind$day[holiday_ind$day_Easter == 1],
               exp_dates$date[exp_dates$holiday == "Easter"])
})


test_that('printing', {
  holiday_rec <- recipe(~ day, test_data_dt) %>%
    step_holiday(all_predictors(), holidays = exp_dates$holiday)
  expect_output(print(holiday_rec))
  expect_output(prep(holiday_rec, training = test_data_dt, verbose = TRUE))
})

