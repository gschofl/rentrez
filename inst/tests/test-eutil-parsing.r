context("Test eutil xml parsing")

a <- .equery('esearch', 'GET', db='pubmed', term='PNAS[ta] AND 97[vi]',
             retstart=6, retmax=6)

test_that(".equery() returns an 'eutil' instance", {
  expect_is(a, 'eutil')
  expect_is(content(a, 'xml'), 'XMLInternalDocument')
})

doc <- xmlRoot(content(a, 'xml'))

test_that("'xvalue() works", {
  character.idList <- xvalue(doc, '/eSearchResult/IdList/*')
  expect_is(character.idList, 'character')
  expect_equal(length(character.idList), 6)
  integer.idList <- xvalue(doc, '/eSearchResult/IdList/*', as='integer')
  expect_is(integer.idList, 'integer')
  expect_equal(length(integer.idList), 6)
  expect_equal(xvalue(doc, '/eSearchResult/Bla'), NA_character_)
  expect_equal(xvalue(doc, '/eSearchResult/Bla', as='integer'), NA_integer_)
  expect_equal(xvalue(doc, '/eSearchResult/Bla', alt=NULL), NULL)
  expect_equal(xvalue(doc, '/eSearchResult/Bla', alt=NULL), NULL)
  expect_equal(xvalue(doc, '/eSearchResult/Bla', alt=''), '')
})

test_that("'xname() works", {
  tagnames <- xname(doc, '/eSearchResult/*')
  expect_is(tagnames, 'character')
  expect_equal(tagnames[1], 'Count')
})

test_that("'xset() works", {
  nodeset <- xset(doc, '/eSearchResult/IdList')
  expect_is(nodeset, 'XMLNodeSet')
})
