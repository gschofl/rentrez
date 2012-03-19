### Esummary ###############################################################
##' @include eutil.r
##' @include utils.r
NULL

##' esummary class
##' 
##' esummary is an S4 class that extends the \code{\link{eutil-class}}.
##' This class provides a container for data retrived by calls to the 
##' NCBI ESummary utility.
##' 
##' esummary objects have two slots in addition to the slots provided by
##' basic \code{\link{eutil-class}} objects:
##' \describe{
##'   \item{database}{The name of the queried database}
##'   \item{documentSummary}{A named list holding the parsed contents of
##'   the XML DocSums returned from a call to the NCBI ESearch utility.}
##' }
##' 
##' @seealso \code{\link{esummary}} for generating calls to the NCBI
##' ESummary utility.
##'
##' @name esummary-class
##' @rdname esummary-class
##' @exportClass esummary
##' @aliases show,esummary-method
##' @aliases $,esummary-method
##' @aliases [,esummary-method
##' @aliases length,esummary-method
##' @aliases docsum,esummary-method
##' @aliases esummary,esummary-method
setClass("esummary",
         representation(database = "character",
                        documentSummary = "list"),
         prototype(database = NA_character_,
                   documentSummary = list()),
         contains = "eutil")

##' @export
setMethod("show",
          signature(object = "esummary"),
          function(object) {
            cat(sprintf("Esummary query using the %s database\n",
                        sQuote(object@database)))
            if (isEmpty(object@documentSummary))
              print(object@data)
            else 
              switch(object@database,
                     protein=print(.docsum.sequence(object)),
                     nuccore=print(.docsum.sequence(object)),
                     nucleotide=print(.docsum.sequence(object)),
                     genome=print(.docsum.genome(object)),
                     pubmed=print(.docsum.pubmed(object)),
                     taxonomy=print(.docsum.taxonomy(object)),
                     print(str(object@documentSummary)))
            return(invisible(NULL))
          })

##' @export
setMethod("$",
          signature(x = "esummary"),
          function (x, name) {
            if (!is.null(x@documentSummary) && !identical(name, "documentSummary"))
              return(slot(x, "documentSummary")[[name, exact=FALSE]])
            else
              return(slot(x, name))
          })

##' @export
setMethod("[",
          signature(x = "esummary" , i = "ANY", j = "missing"),
          function (x, i) {
            if (is(i, "esearch")) {
              return(x@documentSummary[i@idList])
            }
            return(x@documentSummary[i])
          })

##' @export
setMethod("length",
          signature(x = "esummary"),
          function (x) {
            return(length(x@documentSummary))
          })
    
    
##' Access a DocSum from an \code{\link{esummary-class}} object.
##'
##' Attempts to parse ESummary DocSums into a data frame. Returns a
##' named list if not implemented for a specific database.
##'
##' @param object An \code{\link{esummary-class}} object.
##' 
##' @return A data frame or a list
##'
##' @docType methods
##' @examples
##'    ## examples
setGeneric("docsum", function (object) {
  standardGeneric("docsum")
})

##' @export
setMethod("docsum",
          signature(object = "esummary"),
          function (object) {
            switch(object@database,
                   protein=return(.docsum.sequence(object)),
                   nuccore=return(.docsum.sequence(object)),
                   nucleotide=return(.docsum.sequence(object)),
                   genome=return(.docsum.genome(object)),
                   pubmed=return(.docsum.pubmed(object)),
                   taxonomy=return(.docsum.taxonomy(object)),
                   return(object@documentSummary))
          })

##' Retrieve document summaries (DocSums)
##'
##' \code{esummary} retrieves document summaries for a list of primary 
##' UIDs or for a set of UIDs stored in the user's web environment
##' (using the Entrez History server).
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary}{EUtilities}
##' for additional information.
##'
##' @param id (Required)
##' List of UIDs provided either as a character vector, as an
##' \code{\link{esearch-class}} or \code{\link{elink-class}} object,
##' or by reference to a Web Environment and a query key obtained directly
##' from objects returned by previous calls to \code{\link{esearch}},
##' \code{\link{epost}} or \code{\link{elink}}.
##' If UIDs are provided as a plain character vector, \code{db} must be
##' specified explicitly, and all of the UIDs must be from the database
##' specified by \code{db}.
##' @param db (Required only when \code{id} is a character vector of UIDs)
##' Database from which to retrieve DocSums.
##' @param query_key An integer specifying which of the UID lists attached
##' to a user's Web Environment will be used as input to \code{efetch}.
##' (Usually obtained drectely from objects returned by previous
##' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}} calls.)
##' @param WebEnv A character string specifying the Web Environment that
##' contains the UID list. (Usually obtained directely from objects returned
##' by previous \code{\link{esearch}}, \code{\link{epost}} or
##' \code{\link{elink}} calls.)
##' @param retstart Numeric index of the first DocSum to be retrieved
##' (default: 1).
##' @param retmax Total number of DocSums from the input set to be retrieved
##' (maximum: 10,000).
##' @param version If "2.0" \code{esummary} will retrieve version 2.0
##' ESummary XML output but not, currently, attempt to parse it.
##'
##' @return An \code{\link{esummary-class}} object.
##'
##' @export
##'
##' @example inst/examples/esummary.r
esummary <- function (id, 
                      db = NULL,
                      query_key = NULL,
                      WebEnv = NULL,
                      retstart = 1,
                      retmax = 10000,
                      version = "default" ) {
  
  if (missing(id) && is.null(query_key) && is.null(WebEnv))
    stop("No UIDs provided")
  
  if (retmax > 10000)
    stop("Number of DocSums to be downloaded must not exceed 10,000.")
  
  ## get db ################################################################
  # if no db name is provided extract the database name directly from
  # id if it's an esearch, epost or elink object.
  if (is.null(db) && is.null(db <- .getDb(id)))
    stop("No database name provided")
  
  ## get id, or WebEnv and query_key #######################################
  if (is.null(query_key) && is.null(WebEnv)) {
    # extract WebEnv and query_key from id or take the idList/linkList if an
    # esearch/elink object with usehistory=FALSE was provided.
    env_list <- .getId(id)
    WebEnv <- env_list$WebEnv
    query_key <- env_list$query_key
    count <- length(env_list$id)
    id <- env_list$id
  } else {
    # if there is an explicit user provided WebEnv we simply set id=NULL
    # and count=0
    count <- 0
    id <- NULL
  }
  
  if (count > 100) {
    # use HTTP POST if dealing with more than 100 user provided UIDs.
    message(gettextf("%s UIDs were provided. ESummary request uses HTTP POST.",
                     count))
    o <- .httpPOST(eutil="esummary", db=db, id=.collapse(id),
                   query_key=as.character(query_key), WebEnv=WebEnv,
                   retstart=as.character(retstart), retmax=as.character(retmax),
                   version=if (identical(version, "2.0")) "2.0" else NULL)
  }
  else {
    o <- .query(eutil="esummary", db=db, id=.collapse(id),
                query_key=query_key, WebEnv=WebEnv, 
                retstart=retstart, retmax=retmax,
                version=if (identical(version, "2.0")) "2.0" else NULL)
  }

  new("esummary", database=db, error=checkErrors(o),
      url=o@url, data=o@data,
      documentSummary = 
        if (identical(version, "2.0")) 
          NULL 
        else {
          docSums <- lapply(getNodeSet(o@data, '//DocSum'), .parseDocSumItems)
          names(docSums) <- lapply(getNodeSet(o@data, '//DocSum/Id'), xmlValue)
          docSums }
      )
}
