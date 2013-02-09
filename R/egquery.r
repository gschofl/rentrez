#' @include utils.r
#' @include eutil.r
NULL

# egquery-class ----------------------------------------------------------


#' egquery
#' 
#' \dQuote{egquery} is an S4 class that provides a container for data
#' retrived by calls to the NCBI EGQuery utility.
#' 
#' @slot url A character vector containing the query URL.
#' @slot error Any error or warning messages parsed from
#' the output of the call submitted to Entrez.
#' @slot content A \code{\linkS4class{raw}} vector holding the unparsed
#' contents of a request to Entrez.
#' @slot term The search term passed on to the EGQuery Utility.
#' @slot count A data frame with the following columns:
#' \code{dbName}, \code{menuName}, \code{count}, and \code{status}.
#' 
#' @rdname egquery
#' @export
#' @classHierarchy
#' @classMethods
setClass("egquery",
         representation(term = "character",
                        count = "data.frame"),
         prototype(term = NA_character_,
                   count = data.frame()),
         contains = "eutil")


# show-method ------------------------------------------------------------


setMethod("show", "egquery",
          function (object) {
            cat(sprintf("EGQuery for %s\n",
                        sQuote(object@term)))
            print(object@count)
            return(invisible(NULL))
          })



#' \code{egquery} retrieves the number of records in all Entrez databases for
#' a single text query
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EGQuery}{EUtilities}
#' for additional information.
#' 
#' @param term A valid Entrez text query.
#' @return An \code{egquery} instance.
#' @export
#' @example inst/examples/egquery.r
#' @autoImports
egquery <- function (term) {

  if (missing(term))
    stop("No search term provided")
  
  if (length(term) > 1L)
    term <- paste(term, collapse=" OR ")
  
  o <- .query(eutil="egquery", term=term, retmode="xml")
  response <- content(o, "xml")
  term <- xvalue(response, '/Result/Term')
  count <- data.frame(stringsAsFactors=FALSE,
                      dbName = xvalue(response, '//ResultItem/DbName'),
                      menuName = xvalue(response, '//ResultItem/MenuName'),
                      count = xvalue(response, '//ResultItem/Count', as='integer'),
                      status = xvalue(response, '//ResultItem/Status'))
  
  new("egquery", url=queryUrl(o), content=content(o, "raw"),
      error=checkErrors(o), term = term, count = count)
}

