
# Test einfo() -----------------------------------------------------------

context("Testing 'einfo()'")

a <- einfo()
b <- einfo("gene")

test_that("einfo() returns an 'einfoDBList' object", {
  expect_is(a, "einfoDbList")
})

test_that("einfo('dbname') returns an 'einfoDB' object", {
  expect_is(b, "einfoDb")
})

test_that("Subsetting an 'einfoDBList' returns a 'einfoDBList'", {
  expect_is(a[1:10], "einfoDbList")
})

test_that("Subsetting an 'einfoDB' throws an error", {
expect_that(b[1:10], throws_error("object of type 'S4' is not subsettable"))
})

test_that("'content()' returns a character vector or an XMLInternalDocument", {
  expect_that(content(a, as="text"), is_a("character"))
  expect_that(content(b, as="text"), is_a("character"))
  expect_that(content(a, as="xml"), is_a("XMLInternalDocument"))
  expect_that(content(b, as="xml"), is_a("XMLInternalDocument"))
  expect_that(content(a), is_a("XMLInternalDocument"))
  expect_that(content(b), is_a("XMLInternalDocument"))
})

test_that("The accessor 'dbName()' returns a character vector", {
  expect_that(dbName(a), is_a("character"))
  expect_that(dbName(b), is_a("character"))
  expect_that(length(dbName(a[1:10])), equals(10))
})

test_that("The accessor 'queryUrl()' returns the query url", {
  expect_that(queryUrl(a), 
              matches("^http://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo.+"))
  expect_that(queryUrl(b), 
              matches("^http://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo.+"))
})

test_that("The accessor 'error()' returns an 'eutil_error' object", {
  expect_that(error(a), is_a("eutil_error"))
  expect_that(error(b), is_a("eutil_error"))
})


# Test esearch() ---------------------------------------------------------

context("Testing 'esearch()'")

a <- esearch(term="cancer", db="pubmed", reldate=60, datetype="edat",
             retmax=6, usehistory=TRUE)
b <- esearch(term="cancer", db="pubmed", reldate=60, datetype="edat",
             retmax=6, usehistory=FALSE)


test_that("esearch() returns an 'esearch' object", {
  expect_is(a, "esearch")
  expect_is(b, "esearch")
})

test_that("'content()' returns a character vector or an XMLInternalDocument", {
  expect_that(content(a, as="text"), is_a("character"))
  expect_that(content(b, as="text"), is_a("character"))
  expect_that(content(a, as="xml"), is_a("XMLInternalDocument"))
  expect_that(content(b, as="xml"), is_a("XMLInternalDocument"))
  expect_that(content(a), is_a("XMLInternalDocument"))
  expect_that(content(b), is_a("XMLInternalDocument"))
})


test_that("Subsetting an 'esearch' returns an 'esearch' object", {
  expect_that(a[1:2], is_a("esearch"))
  expect_that(b[1:2], is_a("esearch"))
  expect_that(length(b[1:2]), equals(2))
})


test_that("'queryKey', 'webEnv', and 'idList' return the appropriate results", {
  expect_equal(queryKey(a), 1)
  expect_match(webEnv(a), "NCID_+")
  expect_equal(idList(a)[[1]], NA_character_)
  
  expect_equal(webEnv(b), NA_character_)
  expect_equal(queryKey(b), NA_integer_)
  expect_is(idList(b), 'character')
  expect_equal(length(idList(b)), 6)
  
  attribute_of <- function(x) attr(idList(x), "database")
  expect_equal(attribute_of(a), "pubmed")
  expect_equal(attribute_of(b), "pubmed")
})


# Test esummary() --------------------------------------------------------

context("Testing 'esummary()'")

sa <- esummary(a, retstart=3, retmax=2)
sb <- esummary(b[4:5])
sc <- esummary(c(28800982, 28628843), "protein")

test_that("esummary() returns an 'esummary' object", {
  expect_is(sa, "esummary")
  expect_is(sb, "esummary")
  expect_is(sc, "esummary")
})

test_that("'content()' returns a character vector or an XMLInternalDocument", {
  expect_is(content(sc, as="text"), "character")
  expect_is(content(sc, as="xml"), "XMLInternalDocument")
  expect_is(content(sc), "XMLInternalDocument")
})

test_that("'docsum()' returns a list", {
  expect_is(docsum(sa), 'list')
  expect_is(docsum(sb), 'list')
  expect_is(docsum(sc), 'data.frame')
})

test_that("Esummaries retrieved from the History Server or pmids are identical", {
  expect_identical(docsum(sa), docsum(sb))
})

test_that("Subsetting an 'esummary' behaves like a data.frame", {
  expect_is(sc[1:2], 'data.frame')
  expect_is(sc[,1], 'character')
  expect_equal(length(sc[,1]), 2)
})


# Test efetch() ----------------------------------------------------------


context("Testing 'efetch()'")

a <- efetch(c(28800982, 28628843), "protein", "fasta")
b <- efetch(c(28800982, 28628843), "protein", "gp")

test_that("efetch() returns an 'efetch' object", {
  expect_is(a, "efetch")
  expect_is(b, "efetch")
})

test_that("'content()' returns a character vector or an XMLInternalDocument", {
  expect_is(content(a, as="text"), "character")
  expect_is(content(b, as="text"), "character")
  
  expect_is(content(a, as="xml"), "XMLInternalDocument")
  expect_that(content(b, as="xml"), throws_error())
  
  expect_is(content(a), "XMLInternalDocument")
  expect_is(content(b), "character")
  
  expect_that(content(a, as="foo"), throws_error())
  expect_that(content(b, as="foo"), throws_error())
})

test_that("All accessors work as expected", {
  expect_equal(database(a), "protein")
  expect_equal(rettype(a), "fasta")
  expect_equal(retmode(a), "xml")
  expect_equal(queryUrl(a),
               "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=protein&id=28800982%2C28628843&retmode=xml&rettype=fasta&retmax=Inf&tool=Rentrez&email=gschofl%40yahoo.de")
  expect_equal(error(a)$error, NULL)
  expect_equal(error(a)$errmsg, NULL)
  expect_equal(error(a)$wrnmsg, NULL)
})




