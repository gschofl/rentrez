
# eutil-class ------------------------------------------------------------

##' @include utils.r
##' @include blast-classes.r
NULL

#### Old Classes ####
setOldClass("list")
setOldClass("data.frame")

#### Class Unions ####
setClassUnion("XMLOrChar", c("XMLInternalDocument", "character"))
setClassUnion("ListOrFrame", c("data.frame", "list"))
setClassUnion("characterOrNull", c("character", "NULL"))

##' eutil class
##'
##' eutil is an S4 class that is extended by the 
##' \code{\link{einfo-class}}, \code{\link{esearch-class}},
##' \code{\link{esummary-class}}, \code{\link{efetch-class}}, and
##' \code{\link{epost-class}} classes.
##' 
##' These classes provide containers for the data returned from calls to the
##' NCBI Entrez Utilities.
##' Detailed information about the EUtilities provided by NCBI is available
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25501/}{here}.
##'
##' Eutil class objects have three slots:
##' \describe{
##'   \item{url}{A character vector containing the URL submitted to Entrez.}
##'   \item{error}{Any error messages parsed from the output of the
##'   call submitted to Entrez.}
##'   \item{content}{An \code{\link[XML]{XMLInternalDocument-class}} or
##'   character vector holding the unparsed output from the call submitted
##'   to Entrez.}
##' }
##' 
##' @name eutil-class
##' @rdname eutil-class
##' @exportClass eutil
##' @aliases eutil,eutil-method
##' @aliases $,eutil-method
##' @aliases names,eutil-method
.eutil <- 
  setClass("eutil",
           representation(url = "character",
                          error = "list",
                          content = "XMLOrChar"),
           prototype(url = NA_character_,
                     error = list(),
                     content = NA_character_))


# generics and methods ---------------------------------------------------


##' Extract content from an eutil request.
##' 
##' Retrieves the contents of an eutil request either as is, or attempts
##' to return an R object.
##' 
##' @usage content(x, parse = TRUE, format = c('Biostrings', 'DNAbin', 'String'), ...)
##' 
##' @param x an \code{\link{eutil-class}} object.
##' @param parse Parse into an R object where possible.
##' @param format Output format of sequence data.
##' @param ... Further arguments.
##' @export
##' @docType methods
setGeneric("content", function(x, ...) standardGeneric("content"))


##' Extract errors from an eutil request.
##' 
##' @usage error(x)
##' 
##' @param x an \code{\link{eutil-class}} object.
##' @export
##' @docType methods
setGeneric("error", function(x, ...) standardGeneric("error"))


##' @export
setMethod("error", "eutil", function (x) {
  e <- x@error
  if (all(idx <- vapply(e, is.null, logical(1)))) {
    message("No errors")
  } else {
    print(e[!idx])
  }
})


##' Extract url from an eutil request.
##' 
##' @usage query(x)
##' 
##' @param x an \code{\link{eutil-class}} object.
##' @export
##' @docType methods
setGeneric("query", function(x, ...) standardGeneric("query"))

##' @export
setMethod("query", "eutil", function (x) x@url)


##' Container for UIDs and the name of their database
##' 
##' @keywords internal
.idlist <-
  setClass("idlist",
           representation(database = "character",
                          queryKey = "integer",
                          webEnv = "character",
                          count = "numeric",
                          idList = "character"),
           prototype(database = NA_character_,
                     queryKey = NA_integer_,
                     webEnv = NA_character_,
                     count =  NA_integer_,
                     idList = NA_character_))


# show-method ------------------------------------------------------------


##' @export
setMethod("show", "idlist",
          function (object) {
            cat(sprintf("List of UIDs from the %s database.\n",
                        sQuote(object@database)))
            if (is.na(object@queryKey) && !is.na(object@idList)) {
              print(object@idList)
            } else if (!is.na(object@queryKey) && is.na(object@idList)) {
              cat(sprintf("Number of hits: %s\nQuery Key: %s\nWebEnv: %s\n",
                          object@count, object@queryKey, object@webEnv))
            }

            invisible()
          })


# subsetting-method ------------------------------------------------------


##' @export
setMethod("[", c("idlist", "numeric", "missing"),
          function (x, i) {
            .idlist(database = x@database,
                    queryKey = NA_integer_,
                    webEnv = NA_character_,
                    idList = x@idList[i])
          })


# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
