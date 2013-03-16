#' @include all-generics.r
#' @include utils.r
NULL

#### Old Classes ####
setOldClass("list")
setOldClass("eutil_error")
setOldClass("data.frame")


# eutil-class ------------------------------------------------------------


#' eutil
#'
#' \dQuote{eutil} is an S4 class that is extended by the 
#' \code{\linkS4class{einfo}}, \code{\linkS4class{esearch}},
#' \code{\linkS4class{esummary}}, \code{\linkS4class{efetch}},
#' \code{\linkS4class{elink}}, \code{\linkS4class{epost}},
#' and \code{\linkS4class{egquery}} classes.
#' 
#' These classes provide containers for the data returned from calls to the
#' NCBI Entrez Utilities.
#' Detailed information about the EUtilities provided by NCBI is available
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25501/}{here}.
#' 
#' @slot url A character vector containing the query URL.
#' @slot error Any error or warning messages parsed from
#' the output of the call submitted to Entrez.
#' @slot content A character vector holding the unparsed
#' contents of a request to Entrez.
#'  
#' @rdname eutil
#' @export
#' @classHierarchy
#' @classMethods
setClass("eutil",
         representation(url = "character",
                        error = "eutil_error",
                        content = "character"),
         prototype(url = NA_character_,
                   error = list(),
                   content = NA_character_))


setMethod("queryUrl", "eutil", function (x) {
  x@url
})


setMethod("error", "eutil", function (x) {
  x@error
})


#' @S3method
print.eutil_error <- function (x) {
  if (all(is_null(x))) {
    message("No error")
  } else {
    print(x[not_null(x)])
  }
}


get_content <- function (x, as) {
  if (as == "text") {
    x@content
  } else {
    savelyParseXml(x@content)
  }
}


setMethod("content", "eutil",
          function (x, as = "text") {
            as <- match.arg(as, c("text", "xml"))
            get_content(x, as)
          })

