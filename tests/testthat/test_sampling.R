context("Balancing Functions (RUS and ROS)")

library(tm)
library(unbalanced)
library(plyr)


Y <- factor(meta(liu_corpus)$real_label)
X <- liu_dtm
test <- hutch_sampling(X, Y, type = "RUS_under", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)
test_ROS <- hutch_sampling(X, Y, type = "RUS_under", perc = 50, k_pos = 2, w = NULL, verbose = FALSE)
test_new <- test$X
test_new1 <- test_ROS$X
class_test <- test$Y
class_test1 <- test_ROS$Y

test_that("Output of the function in RUS", {
  expect_is(test, 'list')
  expect_is(test$X, c('DocumentTermMatrix', 'simple_triplet_matrix'))
  expect_is(hutch_sampling(test_new, class_test, type = "RUS_under", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$X, c('DocumentTermMatrix',
                                                                                                  'simple_triplet_matrix'))
  expect_is(test$Y, 'factor')
  expect_is(hutch_sampling(test_new, class_test, type = "RUS_under", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$Y, 'factor')

})

test_that("Output Y of the function in ROS", {
  expect_is(test_ROS, 'list')
  expect_is(test_ROS$X, c('DocumentTermMatrix', 'simple_triplet_matrix'))
  expect_is(hutch_sampling(test_new1, class_test1, type = "ROS", perc = 50, k_pos = 2, w = NULL, verbose = FALSE)$X, c('DocumentTermMatrix',
                                                                                                       'simple_triplet_matrix'))
  expect_is(test_ROS$Y, 'factor')
  expect_is(hutch_sampling(test_new, class_test1, type = "ROS", perc = 50, k_pos = 2, w = NULL, verbose = FALSE)$Y, 'factor')

})

test_that("Resampled instances in the resulting X of RUS", {
  expect_equal(nrow(hutch_sampling(X, Y, type = "RUS_under", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$X), 223)
  expect_equal(nrow(hutch_sampling(X, Y, type = "RUS_under", perc = 100, k_pos = 0, w = NULL, verbose = FALSE)$X), 423)
  expect_equal(nrow(hutch_sampling(X, Y, type = "RUS_under", perc = 25, k_pos = 0, w = NULL, verbose = FALSE)$X), 123)
  expect_equal(nrow(hutch_sampling(X, Y, type = "RUS_Pos", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$X), 46)
  expect_equal(nrow(hutch_sampling(X, Y, type = "RUS_Pos", perc = 25, k_pos = 0, w = NULL, verbose = FALSE)$X), 92)
})

test_that("Resampled instances in the resulting X of ROS", {
  expect_equal(nrow(hutch_sampling(X, Y, type = "ROS", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$X), 800)
  expect_equal(nrow(hutch_sampling(X, Y, type = "ROS", perc = 50, k_pos = 1, w = NULL, verbose = FALSE)$X), 423)
  expect_equal(nrow(hutch_sampling(X, Y, type = "ROS", perc = 50, k_pos = 2, w = NULL, verbose = FALSE)$X), 446)

})


test_that("Length of the resulting Response variable", {
  expect_equal(
    length(hutch_sampling(X, Y, type = "RUS_under", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$Y),
    nrow(hutch_sampling(X, Y, type = "RUS_under", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$X)
    )
  expect_equal(
    length(hutch_sampling(X, Y, type = "RUS_under", perc = 25, k_pos = 0, w = NULL, verbose = FALSE)$Y),
    nrow(hutch_sampling(X, Y, type = "RUS_under", perc = 25, k_pos = 0, w = NULL, verbose = FALSE)$X)
    )
  expect_equal(
    length(hutch_sampling(X, Y, type = "RUS_Pos", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$Y),
    nrow(hutch_sampling(X, Y, type = "RUS_Pos", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$X)
    )
  expect_equal(
    length(hutch_sampling(X, Y, type = "RUS_Pos", perc = 25, k_pos = 0, w = NULL, verbose = FALSE)$Y),
    nrow(hutch_sampling(X, Y, type = "RUS_Pos", perc = 25, k_pos = 0, w = NULL, verbose = FALSE)$X)
    )
})

test_that("Frequency of the levels in the resulting Response variable in RUS", {
  expect_equal(count(hutch_sampling(X, Y, type = "RUS_under", perc = 50, k_pos = 0, w = NULL, verbose = FALSE)$Y)[[2]], c(200, 23))
  expect_equal(count(hutch_sampling(X, Y, type = "RUS_Pos", perc = 25, k_pos = 0, w = NULL, verbose = FALSE)$Y)[[2]], c(69, 23))
})

test_that("Frequency of the levels in the resulting Response variable in ROS", {
  expect_equal(count(hutch_sampling(X, Y, type = "ROS", perc = 50, k_pos = 1, w = NULL, verbose = FALSE)$Y)[[2]], c(400, 23))
  expect_equal(count(hutch_sampling(X, Y, type = "ROS", perc = 25, k_pos = 2, w = NULL, verbose = FALSE)$Y)[[2]], c(400, 46))
})
