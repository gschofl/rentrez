#' @include utils.r
#' @include eutil.r
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
#' @slot content A \code{\linkS4class{raw}} vector holding the unparsed
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
                      query_key = NULL, WebEnv = NULL, version = "default") {
  
  ## id may be missing if WebEnv and query_key are provided
  if ((is.null(query_key) || is.null(WebEnv)) && missing(id)) {
    stop("No UIDs provided")
  }
  
  ## if WebEnv and query_key are provided, db must also be provided
  if (not.null(query_key) && not.null(WebEnv) && is.null(db)) {
    stop("No database name provided")
  }
  
  if (retmax > 10000)
    stop("Number of DocSums to be downloaded must not exceed 10,000.")
  
  ## construct list of environment variables
  if (missing(id)) {
    ## if WebEnv and query_key is provided by the user set uid=NULL, count=0, 
    ## retmax stays restricted to 500.
    env_list <- list(WebEnv = WebEnv, query_key = query_key, count = 0,
                     uid = NULL, db = db)
  } else {
    env_list <-.getId(id)
    ## abort if no db was provided and id did not contain db
    db <- db %|null|% env_list$db
    if (is.null(db))
      stop("No database name provided")
  }
  
  o <- if (length(env_list$uid) > 100) {
    # use HTTP POST if dealing with more than 100 user provided UIDs.
    .httpPOST('esummary', db = db, id = .collapse(env_list$uid),
              query_key = env_list$query_key, WebEnv = env_list$WebEnv,
              retstart = as.character(retstart), retmax = as.character(retmax),
              version = if (identical(version, "2.0")) "2.0" else NULL)
  } else {
    .query('esummary', db = db, id = .collapse(env_list$uid),
           query_key = env_list$query_key, WebEnv = env_list$WebEnv, 
           retstart = retstart, retmax = retmax,
           version = if (identical(version, "2.0")) "2.0" else NULL)
  }
  
  new("esummary", url = queryUrl(o), content = content(o, "raw"),
      error = checkErrors(o), database = db, version = version,
      docsum = .docsum(content(o, "xml")))
}

