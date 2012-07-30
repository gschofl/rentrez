##' @include utils.r
##' @include eutil.r
NULL


# esearch-class ----------------------------------------------------------


##' \dQuote{esearch} class
##' 
##' esearch is an S4 class that provides a container for data retrived by
##' calls to the NCBI ESearch utility.
##'
##' @section Slots:
##' \describe{
##'   \item{\code{id}:}{An \code{\linkS4class{idlist}} or
##'   \code{\linkS4class{webenv}} object.}
##'   \item{\code{url}:}{See \code{\linkS4class{eutil}}.}
##'   \item{\code{error}:}{See \code{\linkS4class{eutil}}.}
##'   \item{\code{content}:}{See \code{\linkS4class{eutil}}.}
##' }
##' 
##' @section Extends: 
##'   Class \code{"\linkS4class{eutil}"}, directly.
##'   
##' @param ... arguments passed to the constructor method
##' 
##' @seealso \code{\link{esearch}} for generating calls to the NCBI ESearch
##' utility.
##' 
##' @name esearch-class
##' @rdname esearch-class
##' @exportClass esearch
##' @aliases content,esearch-method
##' @aliases [,esearch-method
##' @aliases length,esearch-method
.esearch <- 
  setClass("esearch",
           representation(id = "webOrId"),
           prototype(id = .idlist()),
           contains = "eutil")


# show-method ------------------------------------------------------------


##' @aliases show,esearch-method
##' @rdname show-methods
setMethod("show", "esearch",
          function (object) {
            
            ## has IdList, hence rettype = "uilist"
            if (is(object@content, "XMLInternalDocument") &&
              !isEmpty(getNodeSet(xmlRoot(object@content), "//IdList"))) { 
              cat(sprintf("ESearch query using the %s database.\nQuery term: %s\n",
                          sQuote(object@id@database), sQuote(object@id@queryTranslation)))
              
              if (is(object@id, "webenv")) {
                cat(sprintf("Number of UIDs stored on the History server: %s\nQuery Key: %s\nWebEnv: %s\n",
                            object@id@count, object@id@queryKey, object@id@webEnv))
              }
              
              if (is(object@id, "idlist")) {
                cat(sprintf("Total number of hits: %s\nNumber of hits retrieved: %s\n",
                            object@id@count, object@id@retMax))
                print(object@id@uid)
              }

              ## show if esearch was performed with rettype = "count"
            } else if (is(object@content, "XMLInternalDocument") &&
              isEmpty(getNodeSet(xmlRoot(object@content), "//IdList"))) {  
              cat(sprintf("ESearch query using the %s database.\nNumber of hits: %s\n",
                          sQuote(object@id@database), object@id@count))
            }
            
            invisible()
          })


# content-method ---------------------------------------------------------


##' @return an \code{\linkS4class{idlist}} object.
##' @rdname esearch-class
##' @rdname content-methods
setMethod("content", "esearch",
          function (x, parse = TRUE) {
            if (isTRUE(parse)) {
              if (is(x@id, "idlist")) {
                if (is.na(x@id@retMax) && !is.na(x@id@count)) {
                  x@id@count
                } else {
                  structure(x@id@uid, database = x@id@database,
                            class = c("idlist","character"))
                }
              } else if (is(x@id, "webenv")) {
                structure(list(webEnv = x@id@webEnv, queryKey = x@id@queryKey),
                          database = x@id@database, class = c("webenv","list"))
              }
            } else {
              x@content
            }
          })


# subsetting-method ------------------------------------------------------


##' @rdname esearch-class
setMethod("[", c("esearch", "numeric", "missing", "ANY"),
          function (x, i, j, ..., drop = TRUE) {
            if (is(x@id, "webenv")) {
              message("Subsetting won't work if USEHISTORY = TRUE")
            } else {
              uids <- x@id@uid[i]
              .idlist(database = x@id@database, retMax = length(uids),
                      retStart = x@id@retStart, count = x@id@count,
                      queryTranslation = x@id@queryTranslation, uid = uids)
            }
          })
  

# length-method ----------------------------------------------------------


##' @rdname esearch-class
setMethod("length", "esearch",
          function (x) x@id@retMax
          )


##' Search and retrieve primary UIDs matching a text query
##'
##' The ESearch utility searches and retrieves primary UIDs for use with
##' \code{\link{efetch}}, \code{\link{esummary}}, and \code{\link{elink}}.
##' \code{esearch} can also post its output set of UIDs to the Entrez history
##' server if the \code{usehistory} parameter is set to \code{TRUE}.
##' The resulting \code{\linkS4class{esearch}} object from either can then be
##' passed on instead of a UID list \code{\link{esummary}},
##' \code{\link{efetch}}, or \code{\link{elink}}.
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}{EUtilities}
##' for additional information.
##' 
##' @param term A valid Entrez text query.
##' @param db Database to search (default: nuccore).
##' @param usehistory If \code{TRUE} search results are stored directly in
##' the user's Web environment so that they can be used in a subsequent 
##' call to \code{\link{esummary}}, \code{\link{efetch}}, or
##' \code{\link{elink}}. Also, \code{usehistory} must be set to \code{TRUE}
##' for \code{esearch} to interpret query key values included in \code{term}
##' or to accept a \code{WebEnv} as input.
##' @param WebEnv Web environment string returned by a previous call to
##' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
##' When provided, \code{esearch} will append the results of the search to
##' the pre-existing WebEnv. Providing WebEnv also allows query keys to be
##' used in \code{term} so that previous search sets can be combined or
##' limited.
##' @param query_key query key returned by a previous call to
##' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
##' When provided, \code{esearch} will find the intersection of the set
##' specified by query_key and the set retrieved by the query in \code{term}
##' (i.e. joins the two with AND).  
##' @param retstart Numeric index of the first UID in the
##' retrieved set to be shown in the XML output (default: 0).
##' @param retmax Total number of UIDs to be retrieved (default: 100).
##' @param rettype Retrieval type. (default: 'uilist', alternative: 'count'.)
##' @param field Optional. Search field used to limit the entire search
##' term.
##' @param datetype Optional. Type of date to limit the search. One of "mdat"
##' (modification date), "pdat" (publication date) or "edat" (Entrez date)
##' @param reldate Optional. Number of days back for which search items are
##' returned.
##' @param mindate Optional. Minimum date of search range. Format
##' YYYY/MM/DD, YYYY/MM, or YYYY.
##' @param maxdate Optional. Maximum date of search range. Format
##' YYYY/MM/DD, YYYY/MM, or YYYY.
##' 
##' @return An \code{\linkS4class{esearch}} object.
##'
##' @export
##' @example inst/examples/esearch.r
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
              retstart=as.character(retstart), retmax=as.character(retmax),
              rettype=rettype, field=field, datetype=datetype,
              reldate=reldate, mindate=mindate, maxdate=maxdate)
  } else {
    .query("esearch", db=db, term=term,
           usehistory=if (usehistory) "y" else NULL,
           WebEnv=WebEnv, query_key=query_key, retstart=retstart,
           retmax=retmax, rettype=rettype, field=field, datetype=datetype,
           reldate=reldate, mindate=mindate, maxdate=maxdate)
  }
  
  retMax <- as.numeric(xmlValue(xmlRoot(o@content)[["RetMax"]]))
  retStart <- as.numeric(xmlValue(xmlRoot(o@content)[["RetStart"]]))
  queryTranslation <- xmlValue(xmlRoot(o@content)[["QueryTranslation"]])
  count <- as.numeric(xmlValue(xmlRoot(o@content)[["Count"]]))
  
  id <- if (usehistory) {
    .webenv(database = db, retMax = retMax, retStart = retStart,
            queryTranslation = queryTranslation, count = count,
            queryKey = as.numeric(xmlValue(xmlRoot(o@content)[["QueryKey"]])),
            webEnv = xmlValue(xmlRoot(o@content)[["WebEnv"]]))
  } else {
    .idlist(database = db, retMax = retMax, retStart = retStart,
            queryTranslation = queryTranslation, count = count,
            uid = as.character(sapply(getNodeSet(o@content, '//Id'), xmlValue)))
  }

  .esearch(url = o@url, content = o@content, error = checkErrors(o), id = id)
}


##' Retrieve the number of records in an Entrez database matching a text
##' query
##'
##' Some additional details about this function
##'
##' @param term A valid NCBI search term.
##' @param db An NCBI database (default = "nuccore").
##' @param ... Additional parameters passed on to \code{\link{esearch}}.
##'
##' @return A numeric vector.
##' @seealso \code{\link{esearch}}.
##' @export
##' @example inst/examples/ecount.r
ecount <- function (term, db = "nuccore", ...) {
  x <- esearch(term=term, db=db, rettype="count", ...)
  cat(sprintf("ESearch query using the %s database.\nNumber of hits: %s\n",
              sQuote(x@id@database), x@id@count))
  return(x@id@count)
}

