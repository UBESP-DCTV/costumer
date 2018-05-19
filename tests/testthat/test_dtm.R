context("Test dtm creation and weighting")

# Constants ====================================================================
data("liu_corpus")

normal <- slam::as.simple_triplet_matrix(
  matrix(c(1, 0, .5, 0, 0, .5), nrow = 3)
)
normal$dimnames$Docs <- c('one', 'two', 'three')
normal$dimnames$Terms <- c('word', 'micromol')
class(normal) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
attr(normal, "weighting") <- c('term frequency', 'tf')

normalized <- slam::as.simple_triplet_matrix(
  matrix(c(1, 0, 3/9, 0, 0, 6/9), nrow = 3)
)
normal$dimnames$Docs <- c('one', 'two', 'three')
normal$dimnames$Terms <- c('word', 'micromol')
class(normalized) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
attr(normalized, "weighting") <- c('term frequency (normalized)', 'tf')

# check_token ==================================================================
test_that("methods works for FALSE", {
  expect_false(check_tokened(liu_corpus))
  expect_false(check_tokened(corpora2list(liu_corpus)))
  expect_false(check_tokened(corpora2list(liu_corpus) %>% unclass))
  expect_false(check_tokened(corpora2list(liu_corpus) %>% unlist))
})

test_that("fails", {
  expect_error(check_tokened(c(1, 2, 3)))
})

# atom_dtm =====================================================================
data_dtm <- atom_dtm(liu_corpus)

test_that("Methods works", {
  expect_is(atom_dtm(c('one', 'two')), 'DocumentTermMatrix')
  expect_is(data_dtm, 'DocumentTermMatrix')
  expect_is(atom_dtm(corpora2list(liu_corpus)), 'DocumentTermMatrix')
  expect_error(atom_dtm(c(1, 2)), 'corpus must be a')
})

test_that("class of dtm", {
  expect_is(atom_dtm(c('one', 'two', 'one two')), 'DocumentTermMatrix')
  expect_is(atom_dtm(c('one', 'two', 'one two')), 'simple_triplet_matrix')
})

test_that("parallel works well", {
  skip('don\'t know how to test on CI for parallel computation')

  expect_equal(
    atom_dtm(c('one', 'two', 'one two')),
    atom_dtm(c('one', 'two', 'one two'), parallel = TRUE)
  )
})

test_that("characters works well", {
  expect_equal(
    dim(atom_dtm(c('one', 'two', 'one two'))),
    c(3, 2)
  )
  expect_equal(
    dim(atom_dtm(c('one', 'two', 'one two'), docs_or_tokens = 'tokens')),
    c(1, 3)
  )
})

test_that("attributes", {
  expect_equal(names(data_dtm$dimnames), c('Docs', 'Terms'))
  expect_equal(length(data_dtm$dimnames$Docs), data_dtm$nrow)
  expect_equal(length(data_dtm$dimnames$Terms), data_dtm$ncol)
})

test_that("results is filled", {
  expect_equal(sum(slam::col_sums(data_dtm)), 121185)
  expect_equal(sum(slam::row_sums(data_dtm)), 121185)
})

# dtm_lfilter ==================================================================

normal_filtered <- dtm_lfilter(normal, liu_dtm)

test_that("class", {
  expect_is(normal_filtered, 'DocumentTermMatrix')
})

test_that("compatibility str", {
  expect_identical(normal_filtered$dimnames$Terms, liu_dtm$dimnames$Terms)
  expect_equal(normal_filtered[, normal_filtered$j]$ncol, 1)
  expect_equal(normal_filtered[, normal_filtered$j]$dimnames$Terms, 'micromol')
  expect_equal(
    normal_filtered[, -which(normal_filtered$dimnames$Docs == 'micromol')]$ncol,
    0
  )
})
