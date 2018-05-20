context("Test dtm conversion")

# Constants ====================================================================
test_df_w <- data.frame(
  a = c(1, 2, 30, 0, 2),
  b = c(0, 0, 0, 10, 3)
)

attr(test_df_w, "weighting") <- c('term frequency (normalized)', 'tf')
class(test_df_w) <- c(class(test_df_w), 'weighted')

# slam::as.simple_triplet_matrix.data.frame ======================================
test_that("class", {
  expect_is(
    slam::as.simple_triplet_matrix(test_df_w), 'DocumentTermMatrix')
})

test_that("correct dimensions", {
  expect_equal(
    slam::as.simple_triplet_matrix(test_df_w) %>% dim,
    test_df_w %>% dim
  )
})

test_that("correct conversion", {
  expect_equal(
    (slam::as.simple_triplet_matrix(test_df_w) == 0) %>% sum,
    (test_df_w == 0) %>% sum
  )
  expect_equal(
    (slam::as.simple_triplet_matrix(test_df_w) != 0) %>% sum,
    (test_df_w != 0) %>% sum
  )
})
