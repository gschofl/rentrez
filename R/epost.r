### Epost ################################################################
##' @include eutil.r
##' @include utils.r
NULL

##' epost class
##' 
##' epost is an S4 class that extends the \code{\link{eutil-class}}.
##' This class provides a container for data retrived by calls to the 
##' NCBI EPnfo utility.
##' 
##' epost objects have three slots in addition to the slots provided by
##' basic \code{\link{eutil-class}} objects:
##' \describe{
##'   \item{database}{The name of the queried database.}
##'   \item{queryKey}{query key parameter specifying the location on the
##'   Entrez history server of the list of UIDs matching the Entrez query.}
##'   \item{webEnv}{Web environment parameter specifying the location on the
##'   Entrez history server of the list of UIDs matching the Entrez query.}
##' }
##' 
##' @seealso \code{\link{epost}} for generating calls to the NCBI EPost
##' utility.
##' 
##' @name post-class
##' @rdname epost-class
##' @exportClass epost
##' @aliases epost, epost-method
##' @aliases show, epost-method
setClass("epost",
         representation(database = "character",
                        queryKey = "numeric",
                        webEnv = "character"),
         prototype(database = NA_character_,
                   queryKey = NA_integer_,
                   webEnv = NA_character_),
         contains = "eutil")

##' @export
setMethod("show",
          signature(object = "epost"),
          function (object) {
            cat(sprintf("EPost upload of %s database UIDs.\nQuery url: %s\nQuery Key: %s\nWeb Environment: %s\n",
                        sQuote(object@database), sQuote(object@url),
                        object@queryKey, sQuote(object@webEnv)))
            return(invisible(NULL))
          })

##' Post a list of primary UIDs to the NCBI history server.
##'
##' \code{epost} posts a list of UIDs for future use the the user's
##' web environment for use with \code{\link{esummary}} or
##' \code{\link{efetch}}.
##' 
##' See the online documentation at
##' \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EPost}
##' for additional information.
##' 
##' @param id A character vector of UIDs.
##' @param db Database containing the UIDs in the input list.
##' @param WebEnv Web Environment. If provided, this parameter specifies the
##' Web Environment that will receive the UIDs sent by \code{epost}.
##' EPost will create a new query key associated with that Web Environment.
##' The WebEnv value is usually returned by a previous call to
##' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
##' If no WebEnv parameter is provided, EPost will create a new Web 
##' Environment and post the UIDs to query_key 1.
##' 
##' @return An \code{\link{epost-class}} object.
##' 
##' @export
epost <- function (id,
                   db=attr(id, "database"),
                   WebEnv=NULL) {
  if (missing(id))
    stop("No UIDs provided")
  if (is.null(db))
    stop("No database name provided")

  hasRes <- FALSE
  ## use WebEnv if available
  if (!is.null(WebEnv)) {
    o <- .query("epost", id=collapseUIDs(id), db=db, WebEnv=WebEnv)
    hasRes <- TRUE
  }
  else if (is(id, "esearch") || is(id, "epost") || is(id, "elink")) {
    if (!is.na(id@webEnv)) {
      o <- .query("epost", id=collapseUIDs(id@idList), db=db, WebEnv=id@webEnv)
      hasRes <- TRUE
    }
    else {
      id <- slot(id, "idList")
    }
  }
  
  if (!hasRes)
    o <- .query("epost", id=collapseUIDs(id), db=db)

  
  new("epost", url=o@url, data=o@data, database=db,
      queryKey=as.numeric(xmlValue(xmlRoot(o@data)[["QueryKey"]])),
      webEnv=as.character(xmlValue(xmlRoot(o@data)[["WebEnv"]])))
  
}
