#' @include utils.r
NULL

#### Old Classes ####
setOldClass("list")
setOldClass("data.frame")

#### Class Unions ####
setClassUnion("XMLOrChar", c("XMLInternalDocument", "character"))
setClassUnion("listOrFrame", c("data.frame", "list"))
setClassUnion("characterOrNull", c("character", "NULL"))


# eutil-class ------------------------------------------------------------


#' \dQuote{eutil} class
#'
#' eutil is an S4 class that is extended by the 
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
#' @section Slots:
#' \describe{
#'   \item{\code{url}:}{A character vector containing the query URL.}
#'   \item{\code{error}:}{Any error or warning messages parsed from
#'   the output of the call submitted to Entrez.}
#'   \item{\code{content}:}{An \code{XMLInternalDocument} object or
#'   a character vector holding the unparsed output from the call
#'   submitted to Entrez.}
#' }
#'   
#' @param ... arguments passed to the constructor method
#' 
#' @name eutil-class
#' @rdname eutil-class
#' @exportClass eutil
#' @aliases eutil,eutil-method
.eutil <- setClass("eutil",
                   representation(url = "character",
                                  error = "list",
                                  content = "XMLOrChar"),
                   prototype(url = NA_character_,
                             error = list(),
                             content = NA_character_))


# content-generic --------------------------------------------------------


#' Extract content from an eutil request.
#' 
#' This function retrieves the data returned by a call to NCBI's Entrez
#' Utilities from the resulting \code{eutil} objects. As default, the function
#' will attempt to parse the contents into an R object.
#' 
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Arguments passed on to methods.
#' 
#' @export
#' @docType methods
#' @rdname content-methods
setGeneric("content", signature="x", function(x, ...) {
  standardGeneric("content")
})


# error-generic ----------------------------------------------------------


#' Extract errors from an eutil request.
#' 
#' @param x an \code{\linkS4class{eutil}} object.
#' @param ... Arguments passed on to methods.
#' @export
#' @docType methods
setGeneric("error", signature="x", function(x, ...) {
  standardGeneric("error")
})


#' @rdname eutil-class
#' @rdname error
setMethod("error", "eutil", function (x) {
  e <- x@error
  if (all(idx <- vapply(e, is.null, logical(1)))) {
    message("No errors")
  } else {
    print(e[!idx])
  }
})


# query-generic -----------------------------------------------------------


#' Extract url from an eutil request.
#' 
#' @param x an \code{\linkS4class{eutil}} object.
#' @param ... Arguments passed on to methods.
#' @export
#' @docType methods
setGeneric("query",  signature="x", function(x, ...) {
  standardGeneric("query")
})


#' @rdname eutil-class
#' @rdname query
setMethod("query", "eutil", function (x) x@url)


# idlist-class -----------------------------------------------------------


#' \dQuote{idlist} class
#' 
#' A container for UIDs
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{database}:}{Database from which the UIDs were retrieved}
#'   \item{\code{retStart}:}{The index of the first UID that is returned
#'   by an \code{esearch} call.}
#'   \item{\code{retMax}:}{The number of UIDs out of the total number of
#'   records that is returned by an \code{esearch} call.}
#'   \item{\code{count}:}{The total number of records matching a query.}
#'   \item{\code{queryTranslation}:}{The search term as translated by the
#'   Entrez search system}
#'   \item{\code{uid}:}{A character vector holding the retrieved UIDs.}
#' }
#' 
#' @keywords internal
#' @name idlist-class
#' @rdname idlist-class
#' @aliases show,idlist-method
#' @aliases [,idlist-method
#' @aliases length,idlist-method
.idlist <- setClass("idlist",
                    representation(database = "character",
                                   retMax = "numeric",
                                   retStart = "numeric",
                                   count = "numeric",
                                   queryTranslation = "character",
                                   uid = "character"),
                    prototype(database = NA_character_,
                              retMax = NA_integer_,
                              retStart = NA_integer_,
                              count = NA_integer_,
                              queryTranslation = NA_character_,
                              uid = NA_character_))


# webenv-class ----------------------------------------------------------


#' \dQuote{webenv} class
#' 
#' A container for web environments
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{database}:}{Database from which the UIDs were retrieved}
#'   \item{\code{retStart}:}{The index of the first UID that is returned
#'   by an \code{esearch} call.}
#'   \item{\code{retMax}:}{The number of UIDs out of the total number of
#'   records that is returned by an \code{esearch} call.}
#'   \item{\code{count}:}{The total number of records matching a query.}
#'   \item{\code{queryTranslation}:}{The search term as translated by the
#'   Entrez search system}
#'   \item{\code{queryKey}:}{Integer query key returned by an esearch call
#'   when \strong{usehistory} is set \code{TRUE}}
#'   \item{\code{webEnv}:}{Web environment string returned from an esearch
#'   call when \strong{usehistory} is set \code{TRUE}}
#' }
#' 
#' @keywords internal
#' @name webenv-class
#' @rdname webenv-class
#' @aliases show,webenv-method
.webenv <- setClass("webenv",
                    representation(database = "character",
                                   retMax = "numeric",
                                   retStart = "numeric",
                                   count = "numeric",
                                   queryTranslation = "character",
                                   queryKey = "numeric",
                                   webEnv = "character"),
                    prototype(database = NA_character_,
                              retMax = NA_integer_,
                              retStart = NA_integer_,
                              count = NA_integer_,
                              queryTranslation = NA_character_,
                              queryKey = NA_integer_,
                              webEnv = NA_character_))


#' @keywords internal
setClassUnion("webOrId", c("webenv", "idlist"))


# show-methods ------------------------------------------------------------


setMethod("show", "idlist",
          function (object) {
            cat(sprintf("List of UIDs from the %s database.\n", sQuote(object@database)))
            print(object@uid)
            invisible()
          })


setMethod("show", "webenv",
          function (object) {
            cat(sprintf("Web Environment for the %s database.\n", sQuote(object@database)))
            cat(sprintf("Number of UIDs stored on the History server: %s\nQuery Key: %s\nWebEnv: %s\n",
                        object@count, object@queryKey, object@webEnv))
            invisible()
          })


# subsetting-method ------------------------------------------------------


#' @rdname idlist-class
setMethod("[", c("idlist", "numeric", "missing", "ANY"),
          function (x, i, j, ..., drop = TRUE) {
            uids <- x@uid[i]
            .idlist(database = x@database, retMax = length(uids),
                    retStart = x@retStart, count = x@count,
                    queryTranslation = x@queryTranslation, uid = uids)
          })


# length-method ----------------------------------------------------------


#' @rdname idlist-class
setMethod("length", "idlist",
          function (x) length(x@uid))

