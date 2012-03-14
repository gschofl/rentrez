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
##' @aliases epost,epost-method
##' @aliases show,epost-method
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
##' web environment for access with \code{\link{esummary}} or
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
                   db=NULL,
                   WebEnv=NULL) {
  
  if (missing(id))
    stop("No UIDs provided")
  
  ## get db ################################################################
  # if no db name is provided extract the database name directly from
  # id if it's an esearch, epost or elink object
  if (is.null(db) && is.null(db <- .getDb(id)))
    stop("No database name provided")

  ## get uids ##############################################################
  env_list <- .getId(id)
  id <- .collapseUIDs(env_list$id)
  if (is.null(id))
    stop("No UIDs provided")

  o <- .query("epost", id=id, db=db,  WebEnv=WebEnv)
  
  new("epost", url=o@url, data=o@data, database=db,
      queryKey=as.numeric(xmlValue(xmlRoot(o@data)[["QueryKey"]])),
      webEnv=as.character(xmlValue(xmlRoot(o@data)[["WebEnv"]])))
  
}
