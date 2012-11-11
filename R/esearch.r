#' @include utils.r
#' @include eutil.r
#' @include uid-list.r
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
#' @slot content An \code{\linkS4class{XMLInternalDocument}} object or
#' a character vector holding the unparsed output from the call
#' submitted to Entrez.
#' @slot uid An \linkS4class{uid} or \linkS4class{webenv} object.
#' 
#' @rdname esearch
#' @export
#' @classHierarchy
#' @classMethods
setClass("esearch",
         representation(uid = "webenvOrUid"),
         prototype(uid = new("uid")),
         contains = "eutil")


# accessor methods -------------------------------------------------------


setMethod("database", "esearch", function(x) database(x@uid))

setMethod("retmax", "esearch", function(x) retmax(x@uid))

setMethod("retstart", "esearch", function(x) retstart(x@uid))

setMethod("count", "esearch", function(x) count(x@uid))

setMethod("queryTranslation", "esearch", function(x) queryTranslation(x@uid))

setMethod("uid", "esearch", function (x) {
  if (is(x@uid, "uid")) {
    uid(x@uid)
  } else if (is(x@uid, "webenv")) {
    message("No UIDs available. Use 'queryKey' and 'webEnv'")
    return(invisible(NULL))
  }
})

setMethod("webEnv", "esearch", function(x) {
  if (is(x@uid, "webenv")) {
    webEnv(x@uid)
  } else if (is(x@uid, "uid")) {
    message("No Web Environment string available. Use 'uid'")
    return(invisible(NULL))
  }
})

setMethod("queryKey", "esearch", function(x) {
  if (is(x@uid, "webenv")) {
    queryKey(x@uid)
  } else if (is(x@uid, "uid"))  {
    message("No Query Key available. Use 'uid'")
    return(invisible(NULL))
  }
})


# show-method ------------------------------------------------------------


#' @autoImports
setMethod("show", "esearch",
          function (object) {
            # has 'IdList', hence rettype = "uilist"
            if (is(object@content, "XMLInternalDocument") &&
                not_empty(getNodeSet(xmlRoot(object@content), "//IdList"))) { 
              cat(sprintf("ESearch query using the %s database.\nQuery term: %s\n",
                          sQuote(database(object)), sQuote(queryTranslation(object))))
              
              if (is(object@uid, "webenv")) {
                cat(sprintf("Number of UIDs stored on the History server: %s\nQuery Key: %s\nWebEnv: %s\n",
                            count(object), queryKey(object), webEnv(object)))
              }
              
              if (is(object@uid, "uid")) {
                cat(sprintf("Total number of hits: %s\nNumber of hits retrieved: %s\n",
                            count(object), retmax(object)))
                print(uid(object))
              }

              # show if esearch was performed with rettype = "count"
            } else if (is(object@content, "XMLInternalDocument") &&
              is_empty(getNodeSet(xmlRoot(object@content), "//IdList"))) {  
              cat(sprintf("ESearch query using the %s database.\nNumber of hits: %s\n",
                          sQuote(database(object)), count(object)))
            } else {
              # fall back to show the 'webenvOrUid' object
              show(object@uid)
            }
            
            invisible()
          })


# content-method ---------------------------------------------------------


#' @autoImports
setMethod("content", "esearch",
          function (x, parse = TRUE) {
            if (isTRUE(parse)) {
              if (is(x@uid, "uid")) {
                if (is.na(retmax(x)) && not.na(count(x))) {
                  count(x)
                } else {
                  new("uid", database = database(x), count = count(x),
                      uid = uid(x))
                }
              } else if (is(x@uid, "webenv")) {
                new("webenv", database = database(x), count = count(x),
                    webEnv = webEnv(x), queryKey = queryKey(x))                    
              }
            } else {
              x@content
            }
          })


# subsetting-method ------------------------------------------------------


#' @autoImports
setMethod("[", c("esearch", "numeric"),
          function (x, i, j, ..., drop = TRUE) {
            x@uid[i]
          })
  

# length-method ----------------------------------------------------------


setMethod("length", "esearch", function (x) length(x@uid))


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
  
  retmax <- as.numeric(xmlValue(xmlRoot(o@content)[["RetMax"]]))
  retstart <- as.numeric(xmlValue(xmlRoot(o@content)[["RetStart"]]))
  queryTranslation <- xmlValue(xmlRoot(o@content)[["QueryTranslation"]])
  count <- as.numeric(xmlValue(xmlRoot(o@content)[["Count"]]))
  
  uid <- if (usehistory) {
    new("webenv",
        database = db, retmax = retmax, retstart = retstart,
        queryTranslation = queryTranslation, count = count,
        queryKey = as.integer(xmlValue(xmlRoot(o@content)[["QueryKey"]])),
        webEnv = xmlValue(xmlRoot(o@content)[["WebEnv"]]))
  } else {
    new("uid",
        database = db, retmax = retmax, retstart = retstart,
        queryTranslation = queryTranslation, count = count,
        uid = as.character(sapply(getNodeSet(o@content, '//Id'), xmlValue)))
  }

  new("esearch", url = o@url, content = o@content, error = checkErrors(o), uid = uid)
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

