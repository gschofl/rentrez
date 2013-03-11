#' @include all-generics.r
#' @include utils.r
NULL

#### Old Classes ####
setOldClass("list")
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
#' @slot content A \code{\linkS4class{raw}} vector holding the unparsed
#' contents of a request to Entrez.
#'  
#' @rdname eutil
#' @export
#' @classHierarchy
#' @classMethods
eutil <- setClass("eutil",
                  representation(url = "character",
                                 error = "list",
                                 content = "raw"),
                  prototype(url = NA_character_,
                            error = list(),
                            content = raw()))


setMethod("queryUrl", "eutil", function (x) x@url)


setMethod("error", "eutil", function (x) {
  e <- x@error
  if (all(is_null(e))) {
    message("No errors")
  } else {
    print(e[not_null(e)])
  }
})


#' @importFrom XML xmlTreeParse
get_content <- function (x, as) {
  if (as == "raw") {
    return( x@content )
  }
  content <- rawToChar(x@content)
  if (as == "text") {
    return( content )
  } else if (as == "xml") {
    tryCatch(xmlTreeParse(content, asText=TRUE, useInternalNodes=TRUE,
                          error=NULL),
             XMLError = function(e) {
               error <- paste("XML parse error:", e$message)
               xmlTreeParse(paste0("<?xml version=\"1.0\"?>\n<ERROR>", error, "</ERROR>"), useInternalNodes=TRUE)
             })
  } 
}


setMethod("content", "eutil",
          function (x, as = "text") {
            as <- match.arg(as, c("text", "xml", "raw"))
            get_content(x, as)
          })

