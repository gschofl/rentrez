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
##'   \item{count}{The number of items posted}
##'   \item{queryKey}{query key parameter specifying the location on the
##'   Entrez history server of the list of UIDs matching the Entrez query.}
##'   \item{webEnv}{Web environment parameter specifying the location on the
##'   Entrez history server of the list of UIDs matching the Entrez query.}
##' }
##' 
##' @seealso \code{\link{epost}} for generating calls to the NCBI EPost
##' utility.
##' 
##' @name epost-class
##' @rdname epost-class
##' @exportClass epost
##' @aliases epost,epost-method
##' @aliases show,epost-method
setClass("epost",
         representation(database = "character",
                        count = "numeric",
                        queryKey = "numeric",
                        webEnv = "character"),
         prototype(database = NA_character_,
                   counr = NA_character_,
                   queryKey = NA_integer_,
                   webEnv = NA_character_),
         contains = "eutil")

##' @export
setMethod("show",
          signature(object = "epost"),
          function (object) {
            cat(sprintf("EPost upload of %s UIDs for database %s.\nQuery Key: %s\nWeb Environment: %s\n",
                        object@count, sQuote(object@database),
                        object@queryKey, sQuote(object@webEnv)))
            return(invisible(NULL))
          })

##' Post a list of primary UIDs to the Entrez history server
##'
##' The EPost utility allows a list of UIDs to be uploaded to the Entrez 
##' History Server and returns an integer label called a query key and
##' an encoded cookie string called a Web environment.
##' \code{\link{epost-class}} objects can then be used in stead of a UID
##' list in subsequent \code{\link{esummary}}, \code{\link{efetch}}, or
##' \code{\link{elink}} calls.
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EPost}{EUtilities}
##' for additional information.
##'
##' @param id (Required) List of UIDs.
##' @param db (Required) Database containing the UIDs in the input list.
##' @param WebEnv (Optional) Web Environment. If provided, this parameter
##' specifies the Web Environment that will receive the UIDs sent by
##' \code{epost}. EPost will create a new query key associated with that
##' Web Environment. The WebEnv value is usually returned by a previous
##' call to \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
##' If no WebEnv parameter is provided, EPost will create a new Web 
##' Environment and post the UIDs to query_key 1.
##' 
##' @return An \code{\link{epost-class}} object.
##' 
##' @export
##' @example /inst/examples/epost.r
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
  if (is.null(id))
    stop("No UIDs provided")
         
  count <- length(env_list$id)
  if (count > 100)
    o <- .httpPOST(eutil="epost", id=.collapse(env_list$id),
                   db=db, WebEnv=WebEnv)
  else
    o <- .query("epost", id=.collapse(env_list$id), db=db, WebEnv=WebEnv)
  
  new("epost", url=o@url, data=o@data, database=db, count=count,
      queryKey=as.numeric(xmlValue(xmlRoot(o@data)[["QueryKey"]])),
      webEnv=as.character(xmlValue(xmlRoot(o@data)[["WebEnv"]])))
  
}
