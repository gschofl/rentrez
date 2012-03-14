### Epost ################################################################
##' @include eutil.r
##' @include utils.r
NULL

##' elink class
##' 
##' elink is an S4 class that extends the \code{\link{eutil-class}}.
##' This class provides a container for data retrived by calls to the 
##' NCBI ELink utility.
##' 
##' epost objects have three slots in addition to the slots provided by
##' basic \code{\link{eutil-class}} objects:
##' \describe{
##'   \item{databaseFrom}{The name of the database containing the input UIDs.}
##'   \item{databaseTo}{The name of the database from which UIDs were retrieved.}
##'   \item{command}{The ELink command mode used for a query.}
##'   \item{idList}{A character vector containing the UIDs sent.}
##'   \item{linkList}{A character vector containing the UIDs returned.}
##' }
##' 
##' @seealso \code{\link{elink}} for generating calls to the NCBI ELink
##' utility.
##' 
##' @name elink-class
##' @rdname elink-class
##' @exportClass elink
##' @aliases elink,elink-method
##' @aliases show,elink-method
setClass("elink",
         representation(databaseFrom = "character",
                        databaseTo = "character",
                        command = "character",
                        queryKey = "numeric",
                        webEnv = "character",
                        idList = "character",
                        linkList = "listOrNULL"),
         prototype(databaseFrom = NA_character_,
                   databaseTo = NA_character_,
                   command = NA_character_,
                   queryKey = NA_integer_,
                   webEnv = NA_character_,
                   idList = NA_character_,
                   linkList = NULL),
         contains = "eutil")

##' @export
setMethod("show",
          signature(object = "elink"),
          function (object) {
            cat(sprintf("ELink from database %s to database %s.\n",
                        sQuote(object@databaseFrom), sQuote(object@databaseTo)))
            cat(sprintf("Query Key: %s\nWeb Environment: %s\n",
                        object@queryKey, object@webEnv))
            cat("IdList:\n")
            print(object@idList)
            cat("LinkList:\n")
            print(object@linkList)
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
elink <- function (id,
                   dbFrom=attr(id, "database"),
                   dbTo=attr(id, "database"),
                   cmd="neighbor",
                   query_key=NULL,
                   WebEnv=NULL,
                   linkname=NULL,
                   term=NULL,
                   holding=NULL,
                   datetype=NULL,
                   reldate=NULL,
                   mindate=NULL,
                   maxdate=NULL) {
  if (missing(id))
    stop("No UIDs provided")
  if (is.null(dbFrom) || is.null(dbTo)) {
    stop("No database names provided")
  }
  if (cmd != "acheck")
    stop(sprintf("%s is not yet supported", sQuote(cmd)))
  
  hasRes <- FALSE
  ## use WebEnv if available
  if (!is.null(WebEnv)) {
    o <- .query("elink", db=dbTo, dbFrom=dbFrom, cmd=cmd, query_key=query_key,
                WebEnv=WebEnv, linkname=linkname, term=term, holding=holding,
                datetype=datetype, reldate=reldate, mindate=mindate,
                maxdate=maxdate)
    hasRes <- TRUE
  }
  else if (is(id, "esearch") || is(id, "epost") || is(id, "elink")) {
    if (!is.na(id@queryKey) && !is.na(id@webEnv)) {
      o <- .query("elink", db=dbTo, dbFrom=dbFrom, cmd=cmd, query_key=id@queryKey,
                  WebEnv=id@webEnv, linkname=linkname, term=term, holding=holding,
                  datetype=datetype, reldate=reldate, mindate=mindate,
                  maxdate=maxdate)
      hasRes <- TRUE
    }
    else {
      id <- slot(id, "idList")
    }
  }
  
  if (!hasRes)
    o <- .query("elink", id=collapseUIDs(id), db=dbTo, dbFrom=dbFrom, cmd=cmd,
                query_key=NULL, WebEnv=NULL, linkname=linkname, term=term,
                holding=holding, datetype=datetype, reldate=reldate,
                mindate=mindate, maxdate=maxdate)
  
  new("elink", url=o@url, data=o@data, error=checkErrors(o),
      databaseFrom=xmlValue(getNodeSet(o@data, "//DbFrom")[[1L]]),
      databaseTo=xmlValue(getNodeSet(o@data, "//DbTo")[[1L]]), command=cmd,
      queryKey=as.numeric(xmlValue(xpathSApply(o@data, "//QueryKey")[[1L]])), 
      webEnv=xmlValue(xpathSApply(o@data, "//WebEnv")[[1L]]),
      idList=sapply(getNodeSet(o@data, "//IdList/Id"), xmlValue),
      linkList=.parseLinkSet(o@data))
}



