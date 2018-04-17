context("Test utilities for tm")


# Constants ====================================================================
corpus         <- data2corpus(liu_4h28, 'id', 'Liu')
train_corpora  <- create_train(liu_4h28, 'Liu')
standard_meta  <- liu_4h28$authors # ID is already present in standard tm metadata
new_meta <- liu_4h28$year # year is not a standard tm metadata so the function
                          # is expect to create that new fild into the meta obj.


# v_assign_meta ================================================================
test_that("meta assignment output class", {
  expect_is(v_assign_meta(corpus, 'author', standard_meta), 'VCorpus')
  expect_is(v_assign_meta(corpus, 'year', new_meta), 'VCorpus')
})

test_that("Metadata assignment", {
  expect_equal(
    NLP::meta(v_assign_meta(corpus, 'author', standard_meta)$content[[95]])[['author']],
    'Devillier, P., et al.'
  )
  expect_equal(
    NLP::meta(v_assign_meta(corpus, 'year', new_meta)$content[[95]])[['year']],
    2015
  )
})

test_that("Meta assignment do not ruin labels", {
  expect_equal(
    names(v_assign_meta(corpus, 'author', standard_meta))[[1]],
    'Liu_1.'
  )
  expect_equal(
    names(v_assign_meta(corpus, 'year', new_meta))[[1]],
    'Liu_1.'
  )
})


# hutch_map ====================================================================
test_that("map output class", {
  expect_is(hutch_map(corpus, tolower), 'VCorpus')
})

test_that("map output content", {
  expect_is(hutch_map(corpus, tolower)$content[[1]], 'character')
  expect_equal(substr(hutch_map(corpus, tolower)$content[[1]], 1, 1), 'o')
})


# corpora2list =================================================================
test_that("output class", {
  expect_is(corpora2list(train_corpora), 'list')
  expect_is(corpora2list(train_corpora), 'corpora_list')
})

test_that("content", {
  expect_equal(
    length(corpora2list(train_corpora)),
    length(train_corpora)
  )
  expect_equal(names(corpora2list(train_corpora)), names(train_corpora))
})

test_that("stored metadata", {
  expect_is(attr(corpora2list(train_corpora), 'metadata'), 'list')
  expect_equal(attr(corpora2list(train_corpora), 'metadata')[[1]][['id']], 'Liu_1.')
  expect_equal(
    names(attr(corpora2list(train_corpora), 'metadata')[[1]]),
    names(train_corpora$content[[1]]$meta)
  )
  expect_is(attr(corpora2list(train_corpora), 'dmeta'), 'data.frame')
  expect_equal(attr(corpora2list(train_corpora), 'dmeta')[['real_label']][[1]], 0)
})



# list2corpora =================================================================

corpora_list <- corpora2list(train_corpora)

test_that("output class", {
  expect_is(list2corpora(corpora_list), 'VCorpus')
})

test_that("correct back procedure", {
  expect_equal(list2corpora(corpora_list), train_corpora)
})

# toktoken =====================================================================

tokened_document <- c('this is', 'is a', 'a beautiful', 'beautiful day')

test_that("tok_token class", {
  expect_is(tok_token(tokened_document), 'list')
  expect_is(tok_token(tokened_document)[[1]], 'character')
})

test_that("tok_tokened results", {
  expect_equal(tok_token(tokened_document)[[1]], c("this", "is"))
  expect_equal(names(tok_token(tokened_document)), tokened_document)
})

# stm2dns  =====================================================================

test_that("stm conversion classes", {
  expect_is(stm2dns(liu_dtm), 'matrix')
  expect_is(as.data.frame.simple_triplet_matrix(liu_dtm), 'data.frame')
  expect_is(stm2dns(liu_dtm), 'weighted')
  expect_is(as.data.frame.simple_triplet_matrix(liu_dtm), 'weighted')
})


test_that("stm conversion attributes", {
  expect_equal(rownames(stm2dns(liu_dtm)), liu_dtm$dimnames$Docs)
  expect_equal(colnames(stm2dns(liu_dtm)), liu_dtm$dimnames$Terms)
  expect_equal(rownames(as.data.frame.simple_triplet_matrix(liu_dtm)), liu_dtm$dimnames$Docs)
  expect_equal(colnames(as.data.frame.simple_triplet_matrix(liu_dtm)), liu_dtm$dimnames$Terms)
})

test_that("reverse", {
  expect_identical(dns2stm(stm2dns(liu_dtm)), liu_dtm)
  expect_equal(
    as.simple_triplet_matrix.data.frame(
      as.data.frame.simple_triplet_matrix(liu_dtm)
    ),
    liu_dtm
  )
})
