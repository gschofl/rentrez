#' @include all-generics.r
#' @include utils.r
NULL

#### Old Classes ####
setOldClass("list")
setOldClass("data.frame")

#### Class Unions ####
setClassUnion("characterOrNull", c("character", "NULL"))
setClassUnion("XMLOrChar", c("XMLInternalDocument", "character"))
setClassUnion("listOrFrame", c("data.frame", "list"))


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
#' @slot content An \code{\linkS4class{XMLInternalDocument}} object or
#' a character vector holding the unparsed output from the call
#' submitted to Entrez.
#'  
#' @rdname eutil
#' @export
#' @classHierarchy
#' @classMethods
.eutil <- setClass("eutil",
                   representation(url = "character",
                                  error = "list",
                                  content = "XMLOrChar"),
                   prototype(url = NA_character_,
                             error = list(),
                             content = NA_character_))


setMethod("error", "eutil", function (x) {
  e <- x@error
  if (all(idx <- vapply(e, is.null, logical(1)))) {
    message("No errors")
  } else {
    print(e[!idx])
  }
})


setMethod("query", "eutil", function (x) x@url)





