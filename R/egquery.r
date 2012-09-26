#' @include utils.r
#' @include eutil.r
NULL

# egquery-class ----------------------------------------------------------


#' \dQuote{egquery} class
#' 
#' egquery is an S4 class that provides a container for data retrived by
#' calls to the NCBI EGQuery utility.
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{url}:}{See \code{\linkS4class{eutil}}.}
#'   \item{\code{error}:}{See \code{\linkS4class{eutil}}.}
#'   \item{\code{content}:}{See \code{\linkS4class{eutil}}.}
#'   \item{\code{term}:}{The search term passed on to the EGQuery Utility}
#'   \item{\code{count}:}{A data frame with the following columns:
#'   \code{dbName}, \code{menuName}, \code{count}, and \code{status}.}
#' }
#' 
#' @section Extends: 
#'   Class \code{\dQuote{\linkS4class{eutil}}}, directly.
#'   
#' @param ... arguments passed to the constructor method
#' 
#' @seealso \code{\link{egquery}} for generating calls to the NCBI EGQuery
#' utility.
#' 
#' @name egquery-class
#' @rdname egquery-class
#' @exportClass egquery
.egquery <- setClass("egquery",
                     representation(term = "character",
                                    count = "data.frame"),
                     prototype(term = NA_character_,
                               count = data.frame()),
                     contains = "eutil")


# show-method ------------------------------------------------------------


#' @aliases show,egquery-method
#' @rdname show-methods
setMethod("show", "egquery",
          function (object) {
            cat(sprintf("EGQuery for %s\n",
                        sQuote(object@term)))
            print(object@count)
            return(invisible(NULL))
          })


#' Retrieve the number of records in all Entrez databases for a single
#' text query
#' 
#' \code{egquery} provides Entrez database counts for a global search.
#' 
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EGQuery}{EUtilities}
#' for additional information.
#' 
#' @param term A valid Entrez text query.
#' 
#' @return An \code{\linkS4class{egquery}} object.
#' 
#' @export
#' @example inst/examples/egquery.r
egquery <- function (term) {

  if (missing(term))
    stop("No search term provided")
  
  if (length(term) > 1L)
    term <- paste(term, collapse=" OR ")
  
  o <- .query(eutil="egquery", term=term, retmode="xml")
  
  .egquery(url=o@url, content=o@content, error=checkErrors(o), 
           term=xmlValue(getNodeSet(xmlRoot(o@content), "/Result/Term")[[1L]]),
           count=data.frame(stringsAsFactors=FALSE,
                            dbName=vapply(getNodeSet(xmlRoot(o@content),
                                                     "//ResultItem/DbName"), xmlValue, character(1)),
                            menuName=vapply(getNodeSet(xmlRoot(o@content),
                                                       "//ResultItem/MenuName"), xmlValue, character(1)),
                            count=as.integer(vapply(getNodeSet(xmlRoot(o@content),
                                                               "//ResultItem/Count"), xmlValue, character(1))),
                            status=vapply(getNodeSet(xmlRoot(o@content),
                                                     "//ResultItem/Status"), xmlValue, character(1))))   
}

