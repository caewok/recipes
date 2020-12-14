library(rsample)
library(dtplyr)

test_that("prepper uses fresh = TRUE", {

  set.seed(123)

  train1 <- mtcars[1:20,]
  train2 <- mtcars[21:32,]

  train1_dt <- lazy_dt(train1)

  rec <- recipe(cyl ~ mpg, train1_dt) %>%
    step_center(mpg)

  prepped_rec <- prep(rec, train1_dt)

  split2 <- initial_split(train2)
  prepped_rec2 <- prepper(split2, prepped_rec)

  expect_equal(
    prepped_rec2$steps[[1]]$means,
    c(mpg = mean(training(split2)$mpg))
  )

})
