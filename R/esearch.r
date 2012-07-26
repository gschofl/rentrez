
# esearch-class ----------------------------------------------------------

##' @include utils.r
##' @include eutil.r
##' @include blast-classes.r
NULL

##' esearch class
##' 
##' esearch is an S4 class that extends the \code{\link{eutil-class}}.
##' This class provides a container for data retrived by calls to the 
##' NCBI ESearch utility.
##'
##' esearch objects have eight slots in addition to the slots provided by
##' basic \code{\link{eutil-class}} objects:
##' \describe{
##'   \item{database}{The name of the queried database.}
##'   \item{count}{An integer giving the total number of hits for a database
##'   query.}
##'   \item{retMax}{An integer giving the number of hits retrieved.}
##'   \item{retStart}{An integer giving the index of the first hit retrieved.}
##'   \item{queryKey}{Parameter specifying a UID list attached to a user's
##'   Web Environment on the Entrez history server.}
##'   \item{webEnv}{Parameter specifying the Web Environment on the Entrez
##'   history server containing the list of UIDs matching an Entrez query.}
##'   \item{queryTranslation}{A Character vector containing the search term as
##'   translated by the Entrez search system}
##'   \item{idList}{A Character vector containing the UIDs returned}
##' }
##' 
##' @seealso \code{\link{esearch}} for generating calls to the NCBI ESearch
##' utility.
##' 
##' @name esearch-class
##' @rdname esearch-class
##' @exportClass esearch
##' @aliases show,esearch-method
##' @aliases [,esearch-method
##' @aliases length,esearch-method
##' @aliases esearch,esearch-method
##' @keywords internal
.esearch <- 
  setClass("esearch",
           representation(database = "character",
                          count = "numeric",
                          retMax = "numeric",
                          retStart = "numeric",
                          queryKey = "numeric",
                          webEnv = "character",
                          queryTranslation = "character",
                          idList = "character"),
           prototype(database = NA_character_, count = NA_integer_,
                     retMax = NA_integer_, retStart = NA_integer_,
                     queryKey = NA_integer_, webEnv = NA_character_,
                     queryTranslation = NA_character_, idList = NA_character_),
           contains = "eutil")


# show-method ------------------------------------------------------------


##' @export
setMethod("show", "esearch",
          function (object) {
            if (is(object@content, "XMLInternalDocument") &&
                !isEmpty(getNodeSet(xmlRoot(object@content), "//IdList"))) {
              ## has IdList, hence rettype = "uilist"
              cat(sprintf("ESearch query using the %s database.\nQuery term: %s\nNumber of hits: %s\n",
                          sQuote(object@database), sQuote(object@queryTranslation),
                          object@count))
              if (!is.na(object@webEnv) && !is.na(object@queryKey))
                cat(sprintf("Query Key: %s\nWebEnv: %s\n",
                            object@queryKey, object@webEnv))
              if (!isEmpty(object@idList) && !is.na(object@idList))
                print(object@idList)
            } else if (is(object@content, "XMLInternalDocument") &&
                     isEmpty(getNodeSet(xmlRoot(object@content), "//IdList"))) {
              ## show if esearch was performed with rettype = "count"
              cat(sprintf("ESearch query using the %s database.\nNumber of hits: %s\n",
                          sQuote(object@database), object@count))
            } else if (is.na(object@content)) {
              ## show after subsetting an esearch object
              cat(sprintf("ESearch query using the %s database.\n",
                          sQuote(object@database)))
              print(object@idList)
            }
            
            return(invisible(NULL))
          })


# content-method ---------------------------------------------------------


setMethod("content", "esearch",
          function (x, parse = TRUE) {
            if (isTRUE(parse)) {
              .idlist(database = as.character(x@database),
                      queryKey = as.integer(x@queryKey),
                      webEnv = as.character(x@webEnv),
                      count = as.numeric(x@count),
                      idList = as.character(x@idList))
            } else {
              x@content
            }
          })


# subsetting-method ------------------------------------------------------


##' @export
setMethod("[", c("esearch", "numeric", "missing"),
          function (x, i, j) {
            if (is.na(x@idList) && !is.na(x@queryKey)) {
              message("Subsetting won't work if USEHISTORY = TRUE")
            } else {
              uid <- x@idList[i]
              .idlist(database = as.character(x@database),
                      count = length(uid),
                      idList = as.character(uid))
            }
          })
  

# length-method ----------------------------------------------------------


##' @export
setMethod("length", "esearch",
          function (x) {
            return(length(x@idList))
          })


##' Search and retrieve primary UIDs matching a text query
##'
##' The ESearch utility searches and retrieves primary UIDs for use with
##' \code{\link{efetch}}, \code{\link{esummary}}, and \code{\link{elink}}.
##' \code{esearch} can also post its output set of UIDs to the Entrez history
##' server if the \code{usehistory} parameter is set to \code{TRUE}.
##' The resulting \code{\link{esearch-class}} object from either can then be
##' passed on in place of a UID list \code{\link{esummary}},
##' \code{\link{efetch}}, or \code{\link{elink}}.
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}{EUtilities}
##' for additional information.
##' 
##' @param term A valid Entrez text query.
##' @param db Database to search (default: nucleotide).
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
##' YYYY/MM/DD.
##' @param maxdate Optional. Maximum date of search range. Format
##' YYYY/MM/DD.
##' 
##' @return An \code{\link{esearch-class}} object.
##'
##' @export
##' @example inst/examples/esearch.r
esearch <- function (term,
                     db="nuccore",
                     usehistory=FALSE,
                     WebEnv=NULL,
                     query_key=NULL,
                     retstart=0,
                     retmax=100,
                     rettype="uilist",
                     field=NULL,
                     datetype=NULL,
                     reldate=NULL,
                     mindate=NULL,
                     maxdate=NULL) {
  if (missing(term))
    stop("No query provided")

  if (length(term) > 1L)
    term <- paste(term, collapse=" OR ")
  
  if (nchar(term) > 100)
    ## for longer search terms use HTTP POST
    o <- .httpPOST(eutil="esearch", db=db, term=.escape(term, httpPOST=TRUE),
                   usehistory=if (usehistory) "y" else NULL,
                   WebEnv=WebEnv, query_key=as.character(query_key),
                   retstart=as.character(retstart), retmax=as.character(retmax),
                   rettype=rettype, field=field, datetype=datetype,
                   reldate=reldate, mindate=mindate, maxdate=maxdate)
  else
    o <- .query(eutil="esearch", db=db, term=term,
                usehistory=if (usehistory) "y" else NULL,
                WebEnv=WebEnv, query_key=query_key, retstart=retstart,
                retmax=retmax, rettype=rettype, field=field, datetype=datetype,
                reldate=reldate, mindate=mindate, maxdate=maxdate)
    
  .esearch(url=o@url, content=o@content, error=checkErrors(o), database=db,
    count=as.numeric(xmlValue(xmlRoot(o@content)[["Count"]])),
    retMax=as.numeric(xmlValue(xmlRoot(o@content)[["RetMax"]])),
    retStart=as.numeric(xmlValue(xmlRoot(o@content)[["RetStart"]])),
    queryKey=as.numeric(xmlValue(xmlRoot(o@content)[["QueryKey"]])),
    webEnv=xmlValue(xmlRoot(o@content)[["WebEnv"]]),
    queryTranslation=xmlValue(xmlRoot(o@content)[["QueryTranslation"]]),
    idList=if (usehistory) NA_character_ else as.character(sapply(getNodeSet(o@content, '//Id'), xmlValue)))
}


##' Retrieve the number of records in an Entrez database matching a text
##' query
##'
##' Some additional details about this function
##'
##' @param term A valid NCBI search term.
##' @param db An NCBI database (default =  "pubmed").
##' @param ... Additional parameters passed on to \code{\link{esearch}}.
##'
##' @return A numeric.
##' @seealso \code{\link{esearch}}.
##' @export
##' @example inst/examples/ecount.r
ecount <- function (term, db = "nuccore", ...) {
  x <- esearch(term=term, db=db, rettype="count", ...)
  cat(sprintf("ESearch query using the %s database.\nNumber of hits: %s\n",
              sQuote(x@database), x@count))
  return(x@count)
}

