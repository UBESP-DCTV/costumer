context('Test Tf-Idf procedures')

# tf_normalizator ==============================================================

## Sample dataset
#
normal <- slam::as.simple_triplet_matrix(
  matrix(c(1, 0, .5, 0, 0, .5), nrow = 3)
)
class(normal) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
attr(normal, "weighting") <- c('term frequency', 'tf')

normal_stm <- normal
class(normal_stm) <- c('simple_triplet_matrix')

to_normalize <- slam::as.simple_triplet_matrix(
  matrix(c(1, 0, 3, 0, 0, 6), nrow = 3)
)
class(to_normalize) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
attr(to_normalize, "weighting") <- c('term frequency', 'tf')

to_normalize_stm <- to_normalize
class(to_normalize_stm) <- c('simple_triplet_matrix')

normalized <- slam::as.simple_triplet_matrix(
  matrix(c(1, 0, 3/9, 0, 0, 6/9), nrow = 3)
)
class(normalized) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
attr(normalized, "weighting") <- c('term frequency (normalized)', 'tf')

normalized_stm <- normalized
class(normalized_stm) <- c('simple_triplet_matrix')

liu_dtm_stm <- liu_dtm
class(liu_dtm_stm) <- c('simple_triplet_matrix')

## tf_normalizator =============================================================

test_that("tf_normalizator behaviour", {
  expect_identical(tf_normalizator(normal), normal)
  expect_equal(tf_normalizator(to_normalize), normalized)
})

test_that("tf_normalizator behaviour with force", {
  expect_error(tf_normalizator(normal_stm), 'dtm has to be a DocumentTermMatrix object')
  expect_identical(tf_normalizator(
    normal_stm,
    force = TRUE
  ), normal_stm)
  expect_equal(
    tf_normalizator(to_normalize_stm, force = TRUE),
    normalized_stm)
})

# idf ==========================================================================

test_that("idf", {
  expect_is(idf(normal), 'numeric')
  expect_is(suppressWarnings(idf(normal_stm, force = TRUE)), 'numeric')

  expect_equal(idf(normal), c(0, log2(3/2)))
  expect_equal(suppressWarnings(idf(normal_stm, force = TRUE)), c(0, log2(3/2)))

  expect_equal(suppressWarnings(idf(normal, sum)), c(1, 1.5))
  expect_equal(suppressWarnings(idf(normal_stm, force = TRUE, sum)), c(1, 1.5))

  expect_error(suppressWarnings(idf(normal, paste)), 'fun has to')
})

# tfidf4dtm ====================================================================

test_that("class and attributes", {
  expect_is(tfidf4dtm(normal), 'DocumentTermMatrix')
  expect_is(tfidf4dtm(normal), 'simple_triplet_matrix')

  expect_error(tfidf4dtm(normal_stm))
  expect_is(suppressWarnings(tfidf4dtm(normal_stm, force = TRUE)), 'simple_triplet_matrix')

  expect_equal(
    attr(tfidf4dtm(normal), 'weighting'),
    c('term frequency - inverse document frequency (normalized)', 'tf-idf')
  )
  expect_equal(
    attr(tfidf4dtm(normal, normalize = FALSE), 'weighting'),
    c('term frequency - inverse document frequency', 'tf-idf')
  )
})

test_that("result", {
  expect_equal(
    tfidf4dtm(to_normalize) %>% as.matrix,
    t(t(as.matrix(normalized)) * c(0, log2(3/2)))
  )
  expect_equal(
    tfidf4dtm(to_normalize, normalize = FALSE) %>% as.matrix,
    t(t(as.matrix(to_normalize)) * c(0, log2(3/2)))
  )
  expect_is(tfidf4dtm(liu_dtm), 'DocumentTermMatrix')
})

test_that("force result", {
  expect_equal(
    suppressWarnings(tfidf4dtm(to_normalize_stm,
      force = TRUE
    )) %>% as.matrix,
    t(t(as.matrix(normalized)) * c(0, log2(3/2)))
  )
  expect_equal(
    suppressWarnings(tfidf4dtm(to_normalize_stm,
      force = TRUE,
      normalize = FALSE
    )) %>% as.matrix,
    t(t(as.matrix(to_normalize)) * c(0, log2(3/2)))
  )
})


# reweights_test ===============================================================


test_that("class and attributes", {
  expect_is(reweights_test(liu_dtm, liu_dtm), 'DocumentTermMatrix')
  expect_is(reweights_test(liu_dtm, liu_dtm), 'simple_triplet_matrix')
  expect_equal(
    attr(reweights_test(liu_dtm, liu_dtm), 'weighting'),
    attr(tfidf4dtm(liu_dtm), 'weighting')
  )
  expect_warning(reweights_test(liu_dtm, liu_dtm, fun = sum))
})

test_that("result", {
  expect_equal(reweights_test(liu_dtm, liu_dtm), tfidf4dtm(liu_dtm))
  expect_equal(
    reweights_test(liu_dtm, liu_dtm, normalize = FALSE),
    tfidf4dtm(liu_dtm, normalize = FALSE)
  )
})


test_that("force result", {
  expect_equal(
    suppressWarnings(reweights_test(
      liu_dtm_stm,
      liu_dtm_stm,
      force = TRUE
    )),
    suppressWarnings(tfidf4dtm(liu_dtm_stm, force = TRUE))
  )
  expect_equal(
    suppressWarnings(reweights_test(
      liu_dtm_stm,
      liu_dtm_stm,
      force = TRUE,
      normalize = FALSE
    )),
    suppressWarnings(tfidf4dtm(liu_dtm_stm,
      force = TRUE,
      normalize = FALSE
    ))
  )
})
