context("Test import data as corpus")

data(liu_4h28)
data(liu_corpus)
data(liu_dtm)

test_that("Class of imported corpus", {
  expect_is(data2corpus(liu_4h28), 'VCorpus')
})

test_that("corpora names equal documents names", {
  expect_equal(
    names(data2corpus(liu_4h28)),
    names(data2corpus(liu_4h28)$content))
})


test_that("Labels of imported corpus", {
  expect_equal(names(data2corpus(liu_4h28, 'id'))[[95]], '95.')
  expect_equal(names(data2corpus(liu_4h28, 'id', 'Liu'))[[95]], 'Liu_95.')
})

test_that("Class of train corpus", {
  expect_is(create_train(liu_4h28), 'VCorpus')
  expect_is(create_train(liu_4h28, 'Liu'), 'VCorpus')
})

test_that("Labels of train corpus", {
  expect_equal(names(create_train(liu_4h28))[[95]], '95.')
  expect_equal(names(create_train(liu_4h28, 'Liu'))[[95]], 'Liu_95.')

})

test_that("meta assignment of train corpus", {
  expect_equal(NLP::meta(create_train(liu_4h28)$content[[95]])[['id']], '95.')
  expect_equal(NLP::meta(create_train(liu_4h28)$content[[95]])[['year']], 2015)
})

test_that("Attributes", {
  expect_equal(names(liu_dtm$dimnames), c('Docs', 'Terms'))
})

