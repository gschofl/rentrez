### Esearch ################################################################

##' reutils formal class (S4) holding ESearch query results
##'
##' \describe{
##'       \item{database}{character vector containing the name of the database}
##'       \item{count}{an integer giving the total number of hits for a database query}
##'       \item{retMax}{an integer giving the number of hits retrieved}
##'       \item{retStart}{an integer giving the index of the first hit retrieved}
##'       \item{queryKey}{not implemented}
##'       \item{webEnv}{not implemented}
##'       \item{queryTranslation}{character vector containing the search term as translated by the Entrez search system}
##'       \item{idList}{character vector containing the UIDs returned}
##'       \item{url}{character vector containig the URL submitted to Entrez}
##'       \item{xml}{XMLInternalDocument holding the unparsed search output}
##' }
##'
##' @seealso \code{\link{eutil}}
##' 
##' @name esearch-class
##' @rdname esearch-class
##' @exportClass esearch
##' 
##' @examples
##' getSlots("esearch")
setClass("esearch",
         representation(database = "charOrNULL",
                        count = "numOrNULL",
                        retMax = "numOrNULL",
                        retStart = "numOrNULL",
                        queryKey = "numOrNULL",
                        webEnv = "charOrNULL",
                        queryTranslation = "charOrNULL",
                        idList = "charOrNULL"),
         prototype(database = NULL, count = NULL, retMax = NULL,
                   retStart = NULL, queryKey = NULL,
                   webEnv = NULL, queryTranslation = NULL,
                   idList = NULL),
         contains = "eutil")


##' Search and retrieve primary UIDs matching a text query.
##'
##' @param term A valid Entrez search query.
##' @param db Database to search (default: nucleotide)
##' @param usehistory If \code{TRUE} search results are retained
##' for future use in the user's environment.
##' @param retstart Index of the first UID in the retrieved set to be
##'   shown in the XML output (default: 0)
##' @param retmax Total number of UIDs to be retrieved (default: 100)
##' @param field Optional. Search field. Limits the entire search term, if used.
##' @param datetype Optional. Type of date to limit the search. One of "mdat"
##'   (modification date), "pdat" (publication date) or "edat" (Entrez date)
##' @param reldate Optional. Number of days back for which search items are
##'   returned.
##' @param mindate Optional. Minimum date of search range. Format
##'   YYYY/MM/DD.
##' @param maxdate Optional. Maximum date of search range. Format
##'   YYYY/MM/DD.
##' 
##' @export
##' @examples 
##'   # Search in Nucleotide for all tRNAs
##'   esearch(term = "biomol trna[prop]", db = "nucleotide")
esearch <- function (term, db="nucleotide",
                     usehistory=FALSE,
                     ## WebEnv=NULL,
                     ## query_key=NULL,
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
  
  if (usehistory) "y" else NULL
  
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
      idList=as.character(sapply(getNodeSet(o@data, '//Id'), xmlValue)))
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


### accessor methods #######################################################

.get.esearch <- function(x, what) {
  switch(what,
         all=x,
         default=list(count = slot(x, "count"),
                      retMax = slot(x, "retMax"),
                      retStart = slot(x, "retStart"),
                      idList = slot(x, "idList"),
                      queryTranslation = slot(x, "queryTranslation")),
         slot(x, what))
}

##' @rdname .get-methods
##' @aliases .get,esearch-method,eutil-method
setMethod(".get",
          signature(x="esearch"),
          function (x, what) {
            .get.esearch(x, what)
          })

### extract methods ########################################################

##' Extract elements of \code{esearch-class} uid lists
##'
##' @rdname extract-methods
##' @aliases [,esearch-method
  setMethod("[",
            signature(x = "esearch", i = "numeric"),
            function (x, i, j, ..., drop = FALSE) {
              new("esearch",
                  database=x@database,
                  queryKey=NA_integer_,
                  webEnv=NA_character_,
                  idList=x@idList[i])
            })

  
### show methods ###########################################################
  
##' @rdname show-methods
##' @aliases show,esearch-method
setMethod("show",
          signature(object="esearch"),
          function (object) {
            if (is.null(slot(object, "count"))) {
              cat(paste("Database: '", slot(object, "database"), "'\n", sep = ""))
              print(slot(object, "idList"))
              return(invisible(NULL))
            }
            else {
              cat(paste("ESearch query using the \'", slot(object, "database"),
                        "\' database.\nQuery term: ", slot(object, "queryTranslation"),
                        "\nNumber of hits: ", slot(object, "count"),
                        "\n", sep = ""))
              print(slot(object, "idList"))
              return(invisible(NULL))
            }
          })

print.esearch <- show
  
  
  
# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
