#' @include utils.r
#' @include eutil.r
NULL


# epost-class ------------------------------------------------------------


#' \dQuote{epost} class
#' 
#' \dQuote{epost} is an S4 class that provides a container for data retrived
#' by calls to the NCBI EPost utility.
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{url}:}{See \code{\linkS4class{eutil}}.}
#'   \item{\code{error}:}{See \code{\linkS4class{eutil}}.}
#'   \item{\code{content}:}{See \code{\linkS4class{eutil}}.}
#'   \item{\code{database}:}{The number of items posted.}
#'   \item{\code{count}:}{The index of the first hit retrieved.}
#'   \item{\code{queryKey}:}{Integer label called query key, specifying the
#'   location on the Entrez history server of the list of UIDs matching the
#'   Entrez query.}
#'   \item{\code{webEnv}:}{Encoded cookie string called a Web environment,
#'   specifying the location on the Entrez history server of the list of UIDs
#'   matching the Entrez query.}
#' }
#' 
#' @section Extends: 
#'   Class \code{"\linkS4class{eutil}"}, directly.
#'   
#' @param ... arguments passed to the constructor method
#' 
#' @seealso \code{\link{esearch}} for generating calls to the NCBI EPost
#' utility.
#' 
#' @name epost-class
#' @rdname epost-class
#' @exportClass epost
#' @aliases content,epost-method
.epost <- 
  setClass("epost",
           representation(database = "character",
                          count = "numeric",
                          queryKey = "numeric",
                          webEnv = "character"),
           prototype(database = NA_character_,
                     count = NA_integer_,
                     queryKey = NA_integer_,
                     webEnv = NA_character_),
           contains = "eutil")


# show-method ------------------------------------------------------------


#' @aliases show,epost-method
#' @rdname show-methods
setMethod("show", "epost",
          function (object) {
            cat(sprintf("EPost upload of %s UIDs for database %s.\nQuery Key: %s\nWeb Environment: %s\n",
                        object@count, sQuote(object@database),
                        object@queryKey, sQuote(object@webEnv)))
            invisible()
          })


# content-method ---------------------------------------------------------


#' @rdname epost-class
#' @rdname content-methods
setMethod("content", "epost",
          function (x, parse = TRUE) {
            if (isTRUE(parse)) {
              structure(list(webEnv = x@webEnv, queryKey = x@queryKey),
                        database = x@database, class = c("webenv","list"))
            } else {
              x@content
            }
          })


#' Post a list of primary UIDs to the Entrez history server
#'
#' The EPost utility allows a list of UIDs to be uploaded to the Entrez 
#' History Server and returns an integer label called a query key and
#' an encoded cookie string called a Web environment.
#' \code{\linkS4class{epost}} objects can then be used in stead of a UID
#' list in subsequent \code{\link{esummary}}, \code{\link{efetch}}, or
#' \code{\link{elink}} calls.
#' 
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EPost}{EUtilities}
#' for additional information.
#'
#' @param id (Required) List of UIDs.
#' @param db (Required) Database containing the UIDs in the input list.
#' @param WebEnv (Optional) Web Environment. If provided, this parameter
#' specifies the Web Environment that will receive the UIDs sent by
#' \code{epost}. EPost will create a new query key associated with that
#' Web Environment. The WebEnv value is usually returned by a previous
#' call to \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
#' If no WebEnv parameter is provided, EPost will create a new Web 
#' Environment and post the UIDs to query_key 1.
#' 
#' @return An \code{\linkS4class{epost}} object.
#' 
#' @export
#' @example /inst/examples/epost.r
epost <- function (id, db = NULL, WebEnv = NULL) {
  
  if (missing(id)) {
    stop("No UIDs provided")
  }
  
  env_list <-.getId(id)
  ## abort if no db was provided and id did not contain db 
  if (is.null(db) && is.null(db <- env_list$db)) {
    stop("No database name provided")
  }
  
  o <- if (length(env_list$uid) > 100) {
    .httpPOST("epost", id=.collapse(env_list$uid), db=db, WebEnv=WebEnv)
  } else {
    .query("epost", id=.collapse(env_list$uid), db=db, WebEnv=WebEnv)
  }
  
  .epost(url=o@url, content=o@content, database=db, count=env_list$count,
         queryKey=as.numeric(xmlValue(xmlRoot(o@content)[["QueryKey"]])),
         webEnv=as.character(xmlValue(xmlRoot(o@content)[["WebEnv"]])))  
}
