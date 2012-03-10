### Esummary ###############################################################

## class "esummary"
setClass("esummary",
         representation(database = "charOrNULL",
                        documentSummary = "listOrNULL"),
         prototype(database = NULL, documentSummary = NULL),
         contains = "eutil")

##' Retrieve document summaries (DocSums)
##'
##' \code{esummary} retrieves document summaries for a list of primary 
##' UIDs. See the online documentation for additional information
##' (\url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary})
##'
##' @param id (Required). List of UIDs provided as a character vector
##'   or an \code{esearch} object. If UIDs are provided
##'   as a plain character vector their database of origin must be 
##'   specified with \code{db}.
##' @param db (Not required if UIDs are provided as an \code{esearch}
##'   object).
##'   Database from which to retrieve DocSums.
##' @param version If "2.0" \code{esummary} will retrieve version 2.0
##'   ESummary XML but currently not parse them.
##'
##' @return A \code{esummary-class} object
##'
##' @export
##'
##' @examples
##'   ##
esummary <- function (id, 
                      db=attr(id, "database"),
                      query_key=NULL,
                      WebEnv=NULL,
                      ## retstart = 1,
                      ## retmax = 100,
                      version = "default" ) {
  if (missing(id) && is.null(query_key) && is.null(WebEnv))
    stop("No UID(s) provided")
  if (is.null(db)) stop("No database name provided")
  
  hasRes <- FALSE
  ## use WebEnv and QueryKey if available
  if (!is.null(query_key) && !is.null(WebEnv)) {
    o <- .query("esummary", db=db, query_key=query_key, WebEnv=WebEnv,
                version=if (identical(version, "2.0")) "2.0" else NULL)
    hasRes <- TRUE
  }
  else if (is(id, "esearch")) {
    if (!is.na(id@queryKey) && !is.na(id@webEnv)) {
      o <- .query("esummary", db=db, query_key=id@queryKey, WebEnv=id@webEnv,
                  version=if (identical(version, "2.0")) "2.0" else NULL)
      hasRes <- TRUE
    }
    else {
      id <- slot(id, "idList")
    }
  }
  
  if (!hasRes) {
    if (length(id) > 1) {
      if (length(id) > 200) {
        warning("The UID list is too large. Only the first 200 UIDs will be used")
        id <- id[1:200]
      }
      id <- paste(id, collapse = ",")
    }
    
    o <- .query("esummary", db=db, id=id, 
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


### accessor methods #######################################################

.get.esummary <- function(x, what) {
  switch(what,
         all=x,
         default=list(database=slot(x, "database"),
                      documentSummary=slot(x, "documentSummary")),
         slot(x, what))
}

##' @rdname .get-methods
##' @aliases .get,esummary-method,eutil-method
setMethod(".get",
          signature(x="esummary"),
          function (x, what) {
            .get.esummary(x, what)
          })


### extract methods ########################################################

##' @rdname extract-methods
##' @aliases $,esummary-method
setMethod("$",
          signature(x="esummary"),
          function (x, name) {
            if (!is.null(x@documentSummary) && !identical(name, "documentSummary"))
              return(slot(x, "documentSummary")[[name, exact=FALSE]])
            else
              return(slot(x, name))
          })

##' Extract elements of \code{esummary} objects
##'
##' @rdname extract-methods
##' @aliases [,esummary-method
setMethod("[",
          signature(x="esummary", i="ANY", j="missing"),
          function (x, i) {
            if (is(i, "esearch")) {
              return(slot(x, "documentSummary")[slot(i, "idList")])
            }
              return(slot(x, "documentSummary")[i])
          })


### show methods ###########################################################

##' @rdname show-methods
##' @aliases show,esummary-method
setMethod("show",
          signature("esummary"),
          function(object) {
            cat(paste("Esummary query using the \'", object@database,
                      "\' database.\n$ documentSummary :\n", sep=""))
            if (is.null(slot(object, "documentSummary")))
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

print.esearch <- show

### summary methods ########################################################

##' @rdname summary-methods
##' @aliases summary,esummary-method
setMethod("summary",
          signature(object = "esummary"),
          function (object, ...) {
            switch(object@database,
                   protein=return(.docsum.sequence(object)),
                   nuccore=return(.docsum.sequence(object)),
                   nucleotide=return(.docsum.sequence(object)),
                   genome=return(.docsum.genome(object)),
                   pubmed=return(.docsum.pubmed(object)),
                   taxonomy=return(.docsum.taxonomy(object)),
                   return(object@documentSummary))
          })




