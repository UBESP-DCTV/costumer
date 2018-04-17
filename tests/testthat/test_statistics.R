context("Statistics of the predicted results")

library(tm)
library(pROC)
library(caret)
data("liu_corpus")

truth <- factor(meta(liu_corpus)$real_label)
sam <- meta(liu_corpus)$real_label
pred <- factor(sample(sam))
ref <- as.numeric(levels(truth))[truth]
test <- statistics(pred, truth,
  positive   = "1",
  dnn        = c("Prediction", "Truth"),
  prevalence = NULL,
  mode       = "everything"
)[[1]]

test_that("class of the output", {

  expect_is(statistics(pred, truth, positive = "1",  dnn = c("Prediction", "Truth"), prevalence = NULL,
                      mode = "everything"), 'list')
  expect_is(statistics(pred, truth, positive = "1",  dnn = c("Prediction", "Truth"), prevalence = NULL,
                       mode = "everything")[[1]], 'confusionMatrix')
  expect_is(statistics(pred, truth, positive = "1",  dnn = c("Prediction", "Truth"), prevalence = NULL,
                       mode = "everything")[[1]]$table, 'table')
})

test_that("outcomes of the output", {

  expect_equal(statistics(pred, truth, positive = "1",  dnn = c("Prediction", "Truth"), prevalence = NULL,
                          mode = "everything")[[1]]$positive, "1")
  expect_equal(statistics(pred, truth, positive = "0",  dnn = c("Prediction", "Truth"), prevalence = NULL,
                          mode = "everything")[[1]]$positive, "0")


})
