#' @include utils.r
#' @include eutil.r
#' @include idlist.r
NULL


# esearch-class ----------------------------------------------------------


#' esearch
#' 
#' \dQuote{esearch} is an S4 class that provides a container for data
#' retrived by calls to the NCBI ESearch utility.
#'
#' @slot url A character vector containing the query URL.
#' @slot error Any error or warning messages parsed from
#' the output of the call submitted to Entrez.
#' @slot content A \code{\linkS4class{raw}} vector holding the unparsed
#' contents of a request to Entrez.
#' @slot idList
#' 
#' @rdname esearch
#' @export
#' @classHierarchy
#' @classMethods
setClass("esearch",
         representation(idList = "idList"),
         prototype(idList = new("idList")),
         contains = "eutil")


# accessor methods -------------------------------------------------------


setMethod("database", "esearch", function(x) database(x@idList))

setMethod("retmax", "esearch", function(x) retmax(x@idList))

setMethod("retstart", "esearch", function(x) retstart(x@idList))

setMethod("count", "esearch", function(x) count(x@idList))

setMethod("queryTranslation", "esearch", function(x) queryTranslation(x@idList))

setMethod("queryKey", "esearch", function (x) queryKey(x@idList))

setMethod("webEnv", "esearch", function (x) webEnv(x@idList))

setMethod("idList", "esearch", function (x, db = TRUE) {
  idList(x@idList, db = db)
})

setMethod("content", "esearch", function(x, as = "xml") {
  callNextMethod(x = x, as = as)
})


# show-method ------------------------------------------------------------


#' @autoImports
setMethod("show", "esearch",
          function (object) {
            response <- xmlRoot(content(object, "xml"))
            # has 'IdList', hence rettype = "uilist"
            if (length(response[["IdList"]]) > 0L) { 
              cat(sprintf("ESearch query using the %s database.\nQuery term: %s\n",
                          sQuote(database(object)),
                          sQuote(queryTranslation(object))))
              
              if (has_webenv(object)) {
                cat(sprintf("Number of UIDs stored on the History server: %s\nQuery Key: %s\nWebEnv: %s\n",
                            count(object), queryKey(object), webEnv(object)))
              } else {
                cat(sprintf("Total number of hits: %s\nNumber of hits retrieved: %s\n",
                            count(object), retmax(object)))
                print(idList(object, FALSE))
              }
              # show if esearch was performed with rettype = "count"
            } else {  
              cat(sprintf("ESearch query using the %s database.\nNumber of hits: %s\n",
                          sQuote(database(object)), count(object)))
            }
            invisible(NULL)
          })


# subsetting-method ------------------------------------------------------


#' @autoImports
setMethod("[", c("esearch", "numeric"),
          function (x, i, j, ..., drop = TRUE) {
            initialize(x, idList = x@idList[i])
          })
  

# length-method ----------------------------------------------------------


setMethod("length", "esearch", function (x) {
  if (has_webenv(x))
    count(x)
  else 
    length(idList(x))
})


#' \code{esearch} searches and retrieves primary UIDs matching a text query
#' for use with \code{\link{efetch}}, \code{\link{esummary}}, and
#' \code{\link{elink}}. \code{esearch} can post its output set of UIDs to the
#' Entrez history server if the \code{usehistory} parameter is set \code{TRUE}.
#' The resulting \code{esearch} object can be passed on to
#' \code{\link{esummary}}, \code{\link{efetch}}, or \code{\link{elink}}.
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}{EUtilities}
#' for additional information.
#' 
#' @param term A valid Entrez text query.
#' @param db Database to search (default: nuccore).
#' @param usehistory If \code{TRUE} search results are stored directly in
#' the user's Web environment so that they can be used in a subsequent 
#' call to \code{\link{esummary}}, \code{\link{efetch}}, or
#' \code{\link{elink}}. Also, \code{usehistory} must be set to \code{TRUE}
#' for \code{esearch} to interpret query key values included in \code{term}
#' or to accept a \code{WebEnv} as input.
#' @param WebEnv Web environment string returned by a previous call to
#' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
#' When provided, \code{esearch} will append the results of the search to
#' the pre-existing WebEnv. Providing WebEnv also allows query keys to be
#' used in \code{term} so that previous search sets can be combined or
#' limited.
#' @param query_key query key returned by a previous call to
#' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
#' When provided, \code{esearch} will find the intersection of the set
#' specified by query_key and the set retrieved by the query in \code{term}
#' (i.e. joins the two with AND).  
#' @param retstart Numeric index of the first UID in the
#' retrieved set to be shown in the XML output (default: 0).
#' @param retmax Total number of UIDs to be retrieved (default: 100).
#' @param rettype Retrieval type. (default: 'uilist', alternative: 'count'.)
#' @param field Optional. Search field used to limit the entire search
#' term.
#' @param datetype Optional. Type of date to limit the search. One of "mdat"
#' (modification date), "pdat" (publication date) or "edat" (Entrez date)
#' @param reldate Optional. Number of days back for which search items are
#' returned.
#' @param mindate Optional. Minimum date of search range. Format
#' YYYY/MM/DD, YYYY/MM, or YYYY.
#' @param maxdate Optional. Maximum date of search range. Format
#' YYYY/MM/DD, YYYY/MM, or YYYY.
#' @return An \code{esearch} instance.
#' @seealso \code{\link{esummary}}, \code{\link{efetch}}, \code{\link{elink}}
#' @export
#' @example inst/examples/esearch.r
#' @autoImports
esearch <- function (term, db = "nuccore", usehistory = FALSE,
                     WebEnv = NULL, query_key = NULL, retstart = 0,
                     retmax = 100, rettype = "uilist", field = NULL,
                     datetype = NULL, reldate = NULL, mindate = NULL,
                     maxdate = NULL) {
  if (missing(term))
    stop("No query provided")

  if (length(term) > 1L)
    term <- paste(term, collapse=" OR ")
  
  o <- if (nchar(term) > 100) {
    ## for longer search terms use HTTP POST
    .httpPOST("esearch", db=db, term=.escape(term, httpPOST=TRUE),
              usehistory=if (usehistory) "y" else NULL,
              WebEnv=WebEnv, query_key=as.character(query_key),
              retstart=as.character(retstart),
              retmax=if (usehistory) "0" else as.character(retmax),
              rettype=rettype, field=field, datetype=datetype,
              reldate=reldate, mindate=mindate, maxdate=maxdate)
  } else {
    .query("esearch", db=db, term=term,
           usehistory=if (usehistory) "y" else NULL,
           WebEnv=WebEnv, query_key=query_key, retstart=retstart,
           retmax=if (usehistory) 0 else retmax, rettype=rettype,
           field=field, datetype=datetype, reldate=reldate,
           mindate=mindate, maxdate=maxdate)
  }
  
  response <- xmlRoot(xmlParse(rawToChar(o@content), useInternalNodes=TRUE))
  retmax <- as.numeric(xmlValue(response[["RetMax"]]))
  retstart <- as.numeric(xmlValue(response[["RetStart"]]))
  queryTranslation <- as.character(xmlValue(response[["QueryTranslation"]]))
  count <- as.numeric(xmlValue(response[["Count"]]))
  queryKey <- as.integer(xmlValue(response[["QueryKey"]]))
  webEnv <- as.character(xmlValue(response[["WebEnv"]]))
  idList <- response[["IdList"]]
  idList <- if (not.null(idList)) {
    vapply(xmlChildren(idList), xmlValue,
           FUN.VALUE=character(1), USE.NAMES=FALSE) %||% NA_character_
  } else {
    NA_character_
  }
  
  uid <- new("idList", database = db, retmax = retmax, retstart = retstart,
             queryTranslation = queryTranslation, count = count,
             queryKey = queryKey, webEnv = webEnv, idList = idList)
  
  new("esearch", url = o@url, content = o@content, error = checkErrors(o),
      idList = uid)
}


# #' Retrieve the number of records in an Entrez database matching a text
# #' query
# #'
# #' Some additional details about this function
# #'
# #' @param term A valid NCBI search term.
# #' @param db An NCBI database (default = "nuccore").
# #' @param ... Additional parameters passed on to \code{\link{esearch}}.
# #'
# #' @return A numeric vector.
# #' @seealso \code{\link{esearch}}.
# #' @export
# #' @example inst/examples/ecount.r
# ecount <- function (term, db = "nuccore", ...) {
#   x <- esearch(term=term, db=db, rettype="count", ...)
#   cat(sprintf("ESearch query using the %s database.\nNumber of hits: %s\n",
#               sQuote(x@id@database), x@id@count))
#   return(x@id@count)
# }

