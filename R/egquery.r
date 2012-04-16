### EGQuery ################################################################
##' @include utils.r
##' @include blast-classes.r
##' @include eutil-classes.r
NULL

##' egquery class
##' 
##' egquery is an S4 class that extends the \code{\link{eutil-class}}.
##' This class provides a container for data retrived by calls to the 
##' NCBI EGQuery utility.
##' 
##' eqquery objects have two slots in addition to the slots provided by
##' basic \code{\link{eutil-class}} objects:
##' \describe{
##'   \item{term}{The search term passed on to the EGQuery Utility}
##'   \item{count}{A data frame with the following columns: \code{dbName},
##'   \code{menuName}, \code{count}, and \code{status}.}
##' }
##' 
##' @seealso \code{\link{egquery}} for generating calls to the NCBI ELink
##' utility.
##' 
##' @name egquery-class
##' @rdname egquery-class
##' @exportClass egquery
##' @aliases egquery,egquery-method
##' @aliases show,egquery-method
setClass("egquery",
         representation(term = "character",
                        count = "data.frame"),
         prototype(term = NA_character_,
                   count = data.frame()),
         contains = "eutil")

##' @export
setMethod("show",
          signature(object = "egquery"),
          function (object) {
            cat(sprintf("EGQuery for %s\n",
                        sQuote(object@term)))
            print(object@count)
            return(invisible(NULL))
          })

##' Retrieve the number of records in all Entrez databases for a single
##' text query
##' 
##' \code{egquery} provides Entrez database counts for a global search.
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EGQuery}{EUtilities}
##' for additional information.
##' 
##' @param term A valid Entrez text query.
##' 
##' @return An \code{\link{egquery-class}} object.
##' 
##' @export
##' @example inst/examples/egquery.r
egquery <- function (term ) {

  if (missing(term))
    stop("No search term provided")
  
  if (length(term) > 1L)
    term <- paste(term, collapse=" OR ")
  
  o <- .query(eutil="egquery", term=term, retmode="xml")
  
  new("egquery", url=o@url, data=o@data,
      error=checkErrors(o), 
      term=xmlValue(getNodeSet(xmlRoot(o@data), "/Result/Term")[[1L]]),
      count=data.frame(stringsAsFactors=FALSE,
                       dbName=vapply(getNodeSet(xmlRoot(o@data),
                    "//ResultItem/DbName"), xmlValue, character(1)),
                       menuName=vapply(getNodeSet(xmlRoot(o@data),
                    "//ResultItem/MenuName"), xmlValue, character(1)),
                       count=as.integer(vapply(getNodeSet(xmlRoot(o@data),
                    "//ResultItem/Count"), xmlValue, character(1))),
                       status=vapply(getNodeSet(xmlRoot(o@data),
                    "//ResultItem/Status"), xmlValue, character(1))))   
}

