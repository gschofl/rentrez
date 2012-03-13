### Esearch ################################################################
##' @include eutil.r
##' @include utils.r
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
##' @aliases esearch,esearch-method
##' @aliases show,esearch-method
##' @aliases [,esearch-method
##' @aliases length,esearch-method
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

### show methods ###########################################################

##' @export
setMethod("show",
          signature(object = "esearch"),
          function (object) {
            if (is.null(object@count)) {
              cat(sprintf("ESearch query using the %s database.\n",
                          sQuote(object@database)))
              if (length(ids <- object@idList) > 0L) {
                print(ids)
              }
              else {
                cat("No hits")
              }
              return(invisible(NULL))
            }
            else {
              cat(sprintf("ESearch query using the %s database.\nQuery term: %s\nNumber of hits: %s\n",
                          sQuote(object@database), sQuote(object@queryTranslation), object@count))
              if (!is.na(object@webEnv) && !is.na(object@queryKey))
                cat(sprintf("Query Key: %s\nWebEnv: %s\n",
                            object@queryKey, object@webEnv))
              if (!all(is.na(object@idList))) {
                print(object@idList)
              }
              return(invisible(NULL))
            }
          })

### extract methods ########################################################

##' @export
setMethod("[",
          signature(x = "esearch", i = "numeric"),
          function (x, i, j, ..., drop = FALSE) {
            new("esearch",
                database=x@database,
                queryKey=NA_integer_,
                webEnv=NA_character_,
                idList=x@idList[i])
          })
  
### length methods #########################################################

##' @export
setMethod("length",
          signature(x = "esearch"),
          function (x) {
            return(length(x@idList))
          })

##' Search and retrieve primary UIDs matching a text query
##'
##' \code{esearch} searches and retrieves primary UIDs for use with
##' \code{\link{efetch}}, \code{\link{esummary}}, and \code{\link{elink}},
##' and optionally stores the results in the user's web environment for
##' future access.
##' 
##' See the online documentation at
##' \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}
##' for additional information.
##' 
##' @param term A valid Entrez text query.
##' @param db Database to search (default: nucleotide).
##' @param usehistory If \code{TRUE} search results are stored drectly in
##' the user's Web Environment so that they can be used in a subsequent 
##' call to \code{\link{efetch}}, \code{\link{esummary}}, or
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
##' @param field Optional. Search field. If used, limits the entire search
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
##' @examples 
##'   # Search in Nucleotide for all tRNAs
##'   esearch(term = "biomol trna[prop]", db = "nucleotide")
esearch <- function (term,
                     db="nucleotide",
                     usehistory=FALSE,
                     WebEnv=NULL,
                     query_key=NULL,
                     retstart=0,
                     retmax=100,
                     field=NULL,
                     datetype=NULL,
                     reldate=NULL,
                     mindate=NULL,
                     maxdate=NULL) {
  if (missing(term))
    stop("No query provided")

  if (length(term) > 1L)
    term <- paste(term, collapse=" OR ")
  
  o <- .query(eutil="esearch", db=db, term=term,
              usehistory=if (usehistory) "y" else NULL, retstart=retstart,
              retmax=retmax, field=field, datetype=datetype,
              reldate=reldate, mindate=mindate, maxdate=maxdate)
    
  new("esearch", url=o@url, data=o@data,
      error=checkErrors(o), database=db,
      count=as.numeric(xmlValue(xmlRoot(o@data)[["Count"]])),
      retMax=as.numeric(xmlValue(xmlRoot(o@data)[["RetMax"]])),
      retStart=as.numeric(xmlValue(xmlRoot(o@data)[["RetStart"]])),
      queryKey=as.numeric(xmlValue(xmlRoot(o@data)[["QueryKey"]])),
      webEnv=xmlValue(xmlRoot(o@data)[["WebEnv"]]),
      queryTranslation=xmlValue(xmlRoot(o@data)[["QueryTranslation"]]),
      idList=if (usehistory) NA_character_ else as.character(sapply(getNodeSet(o@data, '//Id'), xmlValue)))
}


### convenience functions ##################################################

##' Retrieve the number of hits for a query.
##'
##' Some additional details about this function
##'
##' @param term A valid NCBI search term.
##' @param db An NCBI database (default =  "pubmed").
##' @param silent If \code{TRUE} suppress messages.
##' @param ... Additional parameters passed on to \code{\link{esearch}}.
##'
##' @return A numeric.
##' @seealso \code{\link{esearch}}.
##' @export
##' @examples
##'   getCount('\"gene expression\"[title]')
getCount <- function (term, db = "pubmed", silent=FALSE, ...) {
  a <- esearch(term=term, db=db, ...)
  if (!silent)
    cat(paste("ESearch query using the \'", slot(a, "database"),
              "\' database.\nQuery term: ", slot(a, "queryTranslation"),
              "\n", sep = ""))
  return(slot(a, "count"))
}

##' Retrieve a list of UIDs for a query
##'
##' Some additional details about this function
##'
##' @param term A valid NCBI search term.
##' @param db An NCBI database (default = "pubmed").
##' @param silent If \code{TRUE} suppress messages.
##' @param retmax Maximum number of UIDs retrieved.
##'   (defaults to number of hits)
##' @param ... Additional parameters passed on to \code{\link{esearch}}.
##'
##' @return A numeric vector of UIDs.
##' @seealso \code{\link{esearch}}.
##' @export
##' @examples
##'   ##
getUIDs <- function (term,
                     db = "pubmed",
                     silent = FALSE, 
                     retmax = getCount(term, db, TRUE, ...),
                     ...) {
  a <- esearch(term=term, db=db, retmax=retmax, ...)
  if (!silent)
    cat(paste("ESearch query using the \'", slot(a, "database"),
              "\' database.\nQuery term: ", slot(a, "queryTranslation"),
              "\nNumber of hits: ", slot(a, "count"),
              "\nretmax set to ", slot(a, "retMax"),
              "\n", sep = ""))
  new("esearch", database = slot(a, "database"), idList = slot(a, "idList"))
}

  
# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
