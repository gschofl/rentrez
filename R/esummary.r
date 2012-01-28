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
                      db = attr(id, "database"),
                      ## not implemented yet
                      ## query_key = NULL,
                      ## WebEnv = NULL,
                      ## retstart = 1,
                      ## retmax = 100,
                      version = "default" ) {
  if (missing(id)) stop("No UID(s) provided")
  if (is.null(db)) stop("No database name provided")
  if (is(id, "esearch")) id <- slot(id, "idList")
  if (length(id) > 1) {
    if (length(id) > 200) {
      warning("The UID list is too large. Only the first 200
              UIDs will be used")
      id <- id[1:200]
    }
    id <- paste(id, collapse = ",")
  }

  o <- .query("esummary", db=db, id=id, 
              version=if (identical(version, "2.0")) "2.0" else NULL)
  
  new("esummary", database = db, url = slot(o, "url"), xml = slot(o, "xml"),
      documentSummary = 
        if (identical(version, "2.0")) 
          NULL 
        else {
          docSums <- lapply(getNodeSet(slot(o, "xml"), '//DocSum'), .parseDocSumItems)
          names(docSums) <- lapply(getNodeSet(slot(o, "xml"), '//DocSum/Id'), xmlValue)
          docSums }
      )
}


#### Accessor method for esummary
.get.esummary <- function(obj, what) {
  switch(what,
         all=obj,
         default=list(database = slot(obj, "database"),
                      documentSummary = slot(obj, "documentSummary")),
         slot(obj, what))
}
