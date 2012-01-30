##' Search and retrieve primary UIDs matching a text query.
##'
##' @param term A valid Entrez text query
##' @param db Database to search (default: pubmed)
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
esearch <- function (term, db="pubmed",
                     ## not yet implemented
                     ## usehistory = FALSE,
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

  o <- .query(eutil="esearch", db=db, term=term, retstart=retstart,
              retmax=retmax, field=field, datetype=datetype,
              reldate=reldate, mindate=mindate, maxdate=maxdate)
    
  new("esearch", url=slot(o, "url"), xml=slot(o, "xml"),
      error=checkErrors(o), database=db,
      count = as.numeric(xmlValue(xmlRoot(o@xml)[["Count"]])),
      retMax = as.numeric(xmlValue(xmlRoot(o@xml)[["RetMax"]])),
      retStart = as.numeric(xmlValue(xmlRoot(o@xml)[["RetStart"]])),
      queryTranslation = xmlValue(xmlRoot(o@xml)[["QueryTranslation"]]),
      idList = as.character(sapply(getNodeSet(o@xml, '//Id'), xmlValue)))
}


#### Accessor method for esearch
.get.esearch <- function(obj, what) {
  switch(what,
         all=obj,
         default=list(count = slot(obj, "count"),
                      retMax = slot(obj, "retMax"),
                      retStart = slot(obj, "retStart"),
                      idList= slot(obj, "idList"),
                      queryTranslation = slot(obj, "queryTranslation")),
         slot(obj, what))
}

###### Convenience functions

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
