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


# content-generic --------------------------------------------------------


#' Extract content from an eutil request.
#' 
#' This function retrieves the data returned by a call to NCBI's Entrez
#' Utilities from the resulting \code{eutil} objects. As default, the function
#' will attempt to parse the contents into an R object.
#' 
#' @param x An \code{\linkS4class{eutil}} object
#' @param ... Further arguments passed on to methods. 
#' @rdname content
#' @export
#' @genericMethods
setGeneric("content", signature="x", function(x, ...) {
  standardGeneric("content")
})


# error-generic ----------------------------------------------------------


#' Extract errors from an eutil request.
#' 
#' @param x An \code{\linkS4class{eutil}} object
#' @param ... Further arguments passed on to methods. 
#' @rdname error
#' @export
#' @genericMethods
setGeneric("error", signature="x", function(x, ...) {
  standardGeneric("error")
})


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
#' @param x An \code{\linkS4class{eutil}} object
#' @param ... Further arguments passed on to methods. 
#' @rdname query
#' @export
#' @genericMethods
setGeneric("query",  signature="x", function(x, ...) {
  standardGeneric("query")
})


setMethod("query", "eutil", function (x) x@url)


# idlist-class -----------------------------------------------------------


#' idlist
#' 
#' \dQuote{idlist} is an S4 class that provides a container for primary
#' UIDs used by NCBI.
#' 
#' @slot database Database from which the UIDs were retrieved
#' @slot retStart The index of the first UID that is returned
#' by a call to \code{\link{esearch}}.
#' @slot retMax The number of UIDs out of the total number of
#' records that is returned by a call to \code{\link{esearch}}.
#' @slot count The total number of records matching a query.
#' @slot queryTranslation The search term as translated by the
#' Entrez search system
#' @slot uid A character vector holding the retrieved UIDs.
#' 
#' @rdname idlist
#' @export
#' @classHierarchy
#' @classMethods
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


#' webenv
#' 
#' \dQuote{webenv} is an S4 class that provides a container for access
#' information to NCBI web environments.
#' 
#' @slot database Database from which the UIDs were retrieved.
#' @slot retStart The index of the first UID that is returned
#' by a call to \code{\link{esearch}}.
#' @slot retMax The number of UIDs out of the total number of
#' records that is returned by a call to \code{\link{esearch}}.
#' @slot count The total number of records matching a query.
#' @slot queryTranslation The search term as translated by the
#' Entrez search system.
#' @slot queryKey Integer query key returned by an esearch call
#' when \strong{usehistory} is set \code{TRUE}.
#' @slot webEnv Web environment string returned from an esearch
#' call when \strong{usehistory} is set \code{TRUE}}
#' 
#' @rdname webenv
#' @export
#' @classHierarchy
#' @classMethods
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


setMethod("[", c("idlist", "numeric", "missing", "ANY"),
          function (x, i, j, ..., drop = TRUE) {
            uids <- x@uid[i]
            .idlist(database = x@database, retMax = length(uids),
                    retStart = x@retStart, count = x@count,
                    queryTranslation = x@queryTranslation, uid = uids)
          })


# length-method ----------------------------------------------------------


setMethod("length", "idlist", function (x) length(x@uid))

