context('Test preprocessing rutines')

data("liu_corpus")
sample_list_corpus <- list(c('a #', ' b7.', 'b 7. ', 'A.B.  C.'), c('Hello world.'))
sample_stem_corpus <- list(
  c('this is', 'is a', 'a beautiful', 'beautiful day'),
  c('hello beautiful', 'beautiful word')
)



## nonword =====================================================================
#
test_that("nonword class is right", {
  expect_is(nonword(liu_corpus), 'VCorpus')
  expect_is(nonword(sample_list_corpus), 'list')
  expect_is(nonword(unlist(sample_list_corpus)), 'character')
  expect_is(nonword(corpora2list(liu_corpus)), 'corpora_list')
})

test_that("nonword is vectorized", {
  expect_equal(nonword(sample_list_corpus)[[1]][[2]], ' b7 ')
})

test_that("nonword removing numbers", {
  expect_equal(nonword(sample_list_corpus)[[1]][[2]], ' b7 ')
  expect_equal(
    nonword(sample_list_corpus, numbers = TRUE, replacement = '*')[[1]][[3]],
    'b****'
  )
  expect_equal(
    nonword(sample_list_corpus, pattern = 'b', replacement = '*')[[1]][[2]],
    ' *7.'
  )
})


## white =======================================================================
#
test_that("white class is right", {
  expect_is(white(liu_corpus), 'VCorpus')
  expect_is(white(sample_list_corpus), 'list')
  expect_is(white(unlist(sample_list_corpus)), 'character')
  expect_is(white(corpora2list(liu_corpus)), 'corpora_list')
})

test_that("white is vectorized", {
  expect_equal(white(sample_list_corpus)[[1]][[2]], 'b7.')
})

test_that("trim white from start and end and multiple", {
  expect_equal(
    white(sample_list_corpus)[[1]],
    c('a #', 'b7.', 'b 7.', 'A.B. C.')
  )
})

test_that("white manage pattern", {
  expect_equal(
    white(sample_list_corpus, pattern = 'l+')[[2]][[1]],
    'He o wor d.'
  )
})

## lowering ====================================================================
#
test_that("lowering class is right", {
  expect_is(lowering(liu_corpus), 'VCorpus')
  expect_is(lowering(sample_list_corpus), 'list')
  expect_is(lowering(unlist(sample_list_corpus)), 'character')
  expect_is(lowering(corpora2list(liu_corpus)), 'corpora_list')
})

test_that("lowering is vectorized", {
  expect_equal(
    lowering(sample_list_corpus),
    list(c('a #', ' b7.', 'b 7. ', 'a.b.  c.'), c('hello world.'))
  )
})

test_that("lowering lowers", {
  expect_equal(substr(lowering(liu_corpus)$content[[1]]$content, 1, 1), 'o')
})


## stem ========================================================================
#

test_that("stem class is right", {
  expect_is(stem(liu_corpus), 'VCorpus')
  expect_is(stem(sample_list_corpus), 'list')
  expect_is(stem(unlist(sample_list_corpus)), 'character')
  expect_is(stem(corpora2list(liu_corpus)), 'corpora_list')
})

test_that("stem named stemmed result", {
  expect_equal(stem(sample_stem_corpus)[[1]][3], c(`a beautiful` = 'a beauti'))
  expect_equal(length(stem(sample_stem_corpus)), 2)
  expect_equal(length(stem(sample_stem_corpus)[[1]]), 4)
  expect_equal(length(stem(sample_stem_corpus)[[2]]), 2)
})


## ngram =======================================================================
#

test_that("grams class is right", {
  expect_is(ngram(liu_corpus), 'VCorpus')
  expect_is(ngram(sample_list_corpus), 'list')
  expect_is(suppressWarnings(ngram(unlist(sample_list_corpus))), 'list')
  expect_is(
    suppressWarnings(
      ngram(unlist(sample_list_corpus), docs_or_tokens = 'tokens')
    ),
    'list'
  )
  expect_warning(ngram(unlist(sample_list_corpus)), 'Pay attention')
  expect_warning(
    ngram(unlist(sample_list_corpus), docs_or_tokens = 'tokens'),
    'It has not much sense'
  )
  expect_is(ngram(corpora2list(liu_corpus)), 'corpora_list')

  expect_is(bigram(liu_corpus), 'VCorpus')
  expect_is(bigram(sample_list_corpus), 'list')
  expect_is(suppressWarnings(bigram(unlist(sample_list_corpus))), 'list')
  expect_is(
    suppressWarnings(
      bigram(unlist(sample_list_corpus), docs_or_tokens = 'tokens')
    ),
    'list'
  )
  expect_warning(bigram(unlist(sample_list_corpus)), 'Pay attention')
  expect_warning(
    bigram(unlist(sample_list_corpus), docs_or_tokens = 'tokens'),
    'It has not much sense'
  )
  expect_is(bigram(corpora2list(liu_corpus)), 'corpora_list')

  expect_is(trigram(liu_corpus), 'VCorpus')
  expect_is(trigram(sample_list_corpus), 'list')
  expect_is(suppressWarnings(trigram(unlist(sample_list_corpus))), 'list')
  expect_is(
    suppressWarnings(
      trigram(unlist(sample_list_corpus), docs_or_tokens = 'tokens')
    ),
    'list'
  )
  expect_warning(trigram(unlist(sample_list_corpus)), 'Pay attention')
  expect_warning(
    trigram(unlist(sample_list_corpus), docs_or_tokens = 'tokens'),
    'It has not much sense'
  )
  expect_is(trigram(corpora2list(liu_corpus)), 'corpora_list')
})


test_that("grams results are right", {
  expect_equal(ngram(sample_list_corpus)[[2]], c('Hello world', 'Hello', 'world'))
  expect_equal(bigram(sample_list_corpus)[[2]], 'Hello world')
  expect_equal(trigram(sample_list_corpus)[[2]], character(0))
  expect_equal(ngram(liu_corpus)$content[[1]]$content[[1]], 'Oral anti-angiogenesis')
})

# test_that("parallel ngram", {
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#
#   expect_identical(
#     ngram(sample_list_corpus),
#     ngram(sample_list_corpus, parallel = TRUE)
#   )
#   expect_identical(
#     bigram(sample_list_corpus),
#     bigram(sample_list_corpus, parallel = TRUE)
#   )
#   expect_identical(
#     trigram(sample_list_corpus),
#     trigram(sample_list_corpus, parallel = TRUE)
#   )
#
# })
