#' @include utils.r
#' @include eutil.r
#' @include content-parser.r
NULL

setOldClass("list")
setOldClass("data,frame")
setClassUnion("listOrDataFrame", c("list", "data.frame"))

# esummary-class ---------------------------------------------------------
#' esummary
#' 
#' \dQuote{esummary} is an S4 class that provides a container for data
#' retrived by calls to the NCBI ESummary utility.
#' 
#' @slot url A character vector containing the query URL.
#' @slot error Any error or warning messages parsed from
#' the output of the call submitted to Entrez.
#' @slot content A character vector holding the unparsed
#' contents of a request to Entrez.
#' @slot database The name of the queried database.
#' @slot version The version of the document summary requested.
#' @slot docsum The parsed document summaries for a list of input UIDs.
#'
#' @rdname esummary
#' @export
#' @classHierarchy
#' @classMethods
setClass("esummary",
         representation(database = "character",
                        version = "character",
                        docsum = "listOrDataFrame"),
         prototype(database = NA_character_,
                   version = NA_character_,
                   docsum = list()),
         contains = "eutil")


# accessor-methods -------------------------------------------------------

setMethod("database", "esummary", function(x) x@database)

setMethod("docsum", "esummary", function(x) x@docsum)

setMethod("content", "esummary", function(x, as = "xml") {
  callNextMethod(x = x, as = as)
})

# show-method ------------------------------------------------------------


setMethod("show", "esummary",
          function(object) {
            print(docsum(object))
            cat(sprintf("\nEsummary query using the %s database",
                        sQuote(database(object))))
            invisible(NULL)
          })


# subsetting-method ------------------------------------------------------


setMethod("[", "esummary",
          function (x, i, j, ..., drop = TRUE) {
            x <- docsum(x)
            callNextMethod()
          })


setMethod("[[", "esummary",
          function (x, i, j, ...) {
            x <- docsum(x)
            callNextMethod()
          })


#' \code{esummary} retrieves document summaries (DocSums) for a list of
#' primary UIDs or for a set of UIDs stored in the user's web environment
#' (using the Entrez History server).
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary}{EUtilities}
#' for additional information.
#'
#' @param id (Required)
#' List of UIDs provided either as a character vector, as an
#' \code{\linkS4class{esearch}} or \code{\linkS4class{elink}} object,
#' or by reference to a Web Environment and a query key obtained directly
#' from objects returned by previous calls to \code{\link{esearch}},
#' \code{\link{epost}} or \code{\link{elink}}.
#' If UIDs are provided as a plain character vector, \code{db} must be
#' specified explicitly, and all of the UIDs must be from the database
#' specified by \code{db}.
#' @param db (Required only when \code{id} is a character vector of UIDs)
#' Database from which to retrieve DocSums.
#' @param retstart Numeric index of the first DocSum to be retrieved
#' (default: 1).
#' @param retmax Total number of DocSums from the input set to be retrieved
#' (maximum: 10,000).
#' @param query_key An integer specifying which of the UID lists attached
#' to a user's Web Environment will be used as input to \code{efetch}.
#' (Usually obtained drectely from objects returned by previous
#' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}} calls.)
#' @param WebEnv A character string specifying the Web Environment that
#' contains the UID list. (Usually obtained directely from objects returned
#' by previous \code{\link{esearch}}, \code{\link{epost}} or
#' \code{\link{elink}} calls.)
#' @param version If "2.0" \code{esummary} will retrieve version 2.0
#' ESummary XML output.
#' @return An \code{esummary} instance.
#' @export
#' @example inst/examples/esummary.r
#' @autoImports
esummary <- function (id, db = NULL, retstart = 1, retmax = 10000,
                      query_key = NULL, WebEnv = NULL, version = "2.0") {
  
  ## extract query parameters
  params <- get_params(id, db, WebEnv, query_key)
  
  if (retmax > 10000)
    stop("Number of DocSums to be downloaded must not exceed 10,000.")
  
  method <- if (length(params$uid) < 100) "GET" else "POST"
  o <- .equery('esummary', method, db = params$db, id = .collapse(params$uid),
               query_key = params$query_key, WebEnv = params$WebEnv, 
               retstart = retstart, retmax = retmax,
               version = if (version == "2.0") "2.0" else NULL)
  
  error <- error(o)
  error <- if (all_empty(error)) checkErrors(o, FALSE) else error
  if (all_empty(error)) {
    new("esummary", url = queryUrl(o), content = content(o), error = error,
        database = params$db, version = version, docsum = .docsum(x=content(o, "xml")))
  } else {
    new("esummary", url = queryUrl(o), content = content(o), error = error,
        database = params$db, version = version) 
  }
}

