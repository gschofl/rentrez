context("einfo")
## test einfo ====

a <- einfo()

test_that("einfo() returns an 'einfoDBList'", {
  expect_that(a, is_a("einfoDbList"))
})

test_that("Subsetting an 'einfoDBList' returns a character vector", {
  expect_that(a[1:10], is_a("character"))
  expect_that(length(a[1:10]), equals(10))
})

test_that("content() returns a character vector or an XMLInternalDocument", {
  expect_that(content(a), is_a("character"))
  expect_that(content(a, parse=FALSE), is_a("XMLInternalDocument"))
})

test_that("query() returns the query url", {
  expect_that(query(a), 
              matches("^http://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo.+"))
})

test_that("error() returns a 'No errors' message", {
  expect_that(error(a), shows_message("No errors"))
})

b <- einfo("gene")

test_that("einfo(\"gene\") returns an 'einfoDB' object", {
  expect_that(b, is_a("einfoDb"))
})

test_that("content() returns a list or an XMLInternalDocument", {
  expect_that(content(b), is_a("list"))
  expect_that(content(b, parse=FALSE), is_a("XMLInternalDocument"))
})

test_that("query() returns the query url", {
  expect_that(query(b), 
              matches("^http://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo.+db=gene.+"))
  
})

test_that("error() returns a 'No errors' message", {
  expect_that(error(b), shows_message("No errors"))
})
