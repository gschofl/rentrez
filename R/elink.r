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
            
            if (is.na(object@webEnv)) {
              cat("IdList:\n")
              print(object@idList)
              
              cat("Summary of LinkSet:\n")
              lnames <- names(object@linkList)
              llen <- numeric(0)
              for (lname in lnames)
                llen <- c(llen, length(object@linkList[[lname]][["id"]]))
              print(data.frame(LinkName=lnames, LinkCount=llen))
            }
            else {            
              cat(sprintf("Query Key: %s\nWeb Environment: %s\n",
                          object@queryKey, object@webEnv))
            }

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
                   dbFrom=NULL,
                   dbTo=NULL,
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
  
  ## get db ################################################################
  # extract the database names directly from id if it's an esearch, epost or
  # elink object
  if (is.null(dbFrom) && is.null(dbFrom <- .getDb(id)))
    stop("Provide the database containing the input UIDs (dbFrom)")
 
  if (is.null(dbTo) && is.null(dbTo <- .getDb(id)))
    stop("Provide the database from which to retrieve UIDs (dbTo)")
  
  ## get id, or WebEnv and query_key #######################################
  # if no Web Environment is provided extract WebEnv and query_key from id 
  # (or take the idList if an esummary object with usehistory=FALSE was
  # provided)
  if (is.null(query_key) && is.null(WebEnv)) {
    env_list <- .getId(id)
    WebEnv <- env_list$WebEnv
    query_key <- env_list$query_key
    id <- .collapseUIDs(env_list$id)
  } else
    id <- NULL
  
  if (cmd == "acheck")
    stop(sprintf("%s is not yet supported", sQuote(cmd)))
  
  o <- .query("elink", id=id, db=dbTo, dbFrom=dbFrom, cmd=cmd,
              query_key=query_key, WebEnv=WebEnv, linkname=linkname,
              term=term, holding=holding, datetype=datetype,
              reldate=reldate, mindate=mindate, maxdate=maxdate)
  
  queryKey <-
    if (length(qk <- xpathSApply(o@data, "//QueryKey")) > 0L)
      as.integer(xmlValue(qk[[1L]]))
    else
      NA_integer_
  
  webEnv <- 
    if (length(we <- xpathSApply(o@data, "//WebEnv")) > 0L)
      xmlValue(we[[1L]])
  else
    NA_character_
  
  new("elink", url=o@url, data=o@data, error=checkErrors(o),
      databaseFrom=xmlValue(getNodeSet(o@data, "//DbFrom")[[1L]]),
      databaseTo=xmlValue(getNodeSet(o@data, "//DbTo")[[1L]]), command=cmd,
      queryKey=queryKey, webEnv=webEnv,
      idList=sapply(getNodeSet(o@data, "//IdList/Id"), xmlValue),
      linkList=.parseLinkSet(o@data))
}



