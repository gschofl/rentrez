##' @include utils.r
##' @include eutil.r
NULL


# elink-class ------------------------------------------------------------


##' \dQuote{elink} class
##' 
##' \dQuote{elink} is an S4 class that provides a container for data retrived
##' by calls to the NCBI ELink utility.
##' 
##' @section Slots:
##' \describe{
##'   \item{\code{url}:}{See \code{\linkS4class{eutil}}.}
##'   \item{\code{error}:}{See \code{\linkS4class{eutil}}.}
##'   \item{\code{content}:}{See \code{\linkS4class{eutil}}.}
##'   \item{\code{id}:}{An \code{\linkS4class{idlist}} object.}
##'   \item{\code{databaseTo}:}{}
##'   \item{\code{command}:}{}
##'   \item{\code{queryKey}:}{}
##'   \item{\code{webEnv}:}{}
##'   \item{\code{linkset}:}{A list containing the linked data.}
##' }
##' 
##' @section Extends: 
##'   Class \code{"\linkS4class{eutil}"}, directly.
##'   
##' @param ... arguments passed to the constructor method
##' 
##' @seealso \code{\link{elink}} for generating calls to the NCBI ELink
##' utility.
##' 
##' @name elink-class
##' @rdname elink-class
##' @exportClass elink
##' @aliases [,elink-method
.elink <- 
  setClass("elink",
           representation(id = "webOrId",
                          databaseTo = "character",
                          command = "character",
                          queryKey = "numeric",
                          webEnv = "character",
                          linkset = "listOrFrame"),
           prototype(id = .idlist(),
                     databaseTo = NA_character_,
                     command = NA_character_,
                     queryKey = NA_integer_,
                     webEnv = NA_character_,
                     linkset = list()),
           contains = "eutil")


# show-method ------------------------------------------------------------


##' @aliases show,elink-method
##' @rdname show-methods
setMethod("show", "elink",
          function (object) {
            if (object@command == "acheck")
              .show.acheck(object)
            else if (object@command == "ncheck")
              .show.ncheck(object)
            else if (object@command == "lcheck")
              .show.lcheck(object)
            else if (object@command %in% c("llinks","llinkslib","prlinks"))
              .show.llinks(object)
            else {
              cat(sprintf("ELink from database %s to database %s.\n",
                          sQuote(object@id@database), sQuote(object@databaseTo)))
              
              if (is(object@id, "idlist")) {
                cat("IdList:\n")
                print(object@id@uid)
              } else if (is(object@id, "webenv")){            
                cat(sprintf("Query Key: %s\nWeb Environment: %s\n",
                            object@id@queryKey, object@id@webEnv))
              }
              
              cat("Summary of LinkSet:\n")
              lnames <- names(object@linkset)
              llen <- numeric(0)
              for (lname in lnames)
                llen <- c(llen, length(object@linkset[[lname]][["id"]]))
              print(data.frame(LinkName=lnames, LinkCount=llen))
              
              invisible()
            }
          })


#' @keywords internal
.show.acheck <- function (object) {
  cat("ELink list of possible links for a set of UIDs:\n")
  print(object@linkset)
  invisible()
}


#' @keywords internal
.show.ncheck <- function (object) {
  cat("Existence of links within the same database for a set of UIDs\n")
  print(object@linkset)
  invisible()
}


#' @keywords internal
.show.lcheck <- function (object) {
  cat("Existence of external links for a set of UIDs\n")
  print(object@linkset)
  invisible()
}


#' @keywords internal
.show.links <- function (object) {
  cat("External links for UID:")
  w <- getOption("width")
  x <- lapply(object@linkset, function(o) {
    cat(sprintf("\n\nUID %s\n", attr(o, "id")))
    url <- c("Url", o[["url"]])
    cat <- c("Category", o[["category"]])
    provider <- c("Provider", o[["provider.nameAbbr"]])
    
    char.prov <- max(nchar(provider))
    char.cat <- max(nchar(cat))
    char.url <- round(w*0.5)
    trim <- ifelse(char.prov + char.cat + char.url + 12 > w,
                   w - char.prov - char.cat - 12, char.url)
    cat(paste0(provider, blanks(char.prov - nchar(provider) + 4),
               cat, blanks(char.cat - nchar(cat) + 4),
               paste0(strtrim(url, trim), " ..."), collapse="\n"))
  })

  invisible()
}


# content-method ---------------------------------------------------------


##' @rdname elink-class
##' @rdname content-methods
setMethod("content", "elink",
          function (x, parse = TRUE) {
            if (isTRUE(parse)) {
              if (is.na(x@webEnv) || is.na(x@queryKey)) {
                x@linkset
              } else {
                structure(list(webEnv = x@id@webEnv, queryKey = x@id@queryKey),
                          database = x@id@database, class = c("webenv","list"))
              } 
            } else {
              x@content
            }
          })


# subsetting-method ------------------------------------------------------


##' @rdname elink-class
setMethod("[", c("elink", "ANY", "missing"),
          function (x, i, j, ..., drop = TRUE) {
            if (is(x@id, "webenv")) {
              message("Subsetting won't work if USEHISTORY = TRUE")
            } else {
              uids <- unlist(x@link[i][[1L]][["id"]], use.names=FALSE)
              .idlist(database = x@id@database,
                      uid = uids,
                      count = length(uids))
            }
          })


##' Retrieve UIDs linked to an input set of UIDs.
##'
##' \code{elink} generates a list of UIDs in a specified Entrez database
##' that are linked to a set of input UIDs in either the same or another
##' database. For instance, the ELink utility can find Entrez gene records
##' linked to records in Entrez Protein.
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ELink}{EUtilities}
##' for additional information.
##' 
##' If \code{dbTo} and \code{dbFrom} are set to the same database, ELink will
##' return neighbors within that database.
##' 
##' Elink commands (cmd) specify the function that elink will perform.
##' Available commands are:
##' \itemize{
##'   \item{"\strong{neighbor}" }{(Default) ELink returns a set of UIDs in dbTo
##'   linked to the input UIDs in dbFrom.}
##'   \item{"\strong{neighbor_score}" }{ELink returns a set of UIDs within the
##'   same database as the input UIDs along with similarity scores.}
##'   \item{"\strong{neighbor_history}" }{ELink posts the output UIDs to the
##'   Entrez History server and returns a query_key and WebEnv parameter.
##'   Alternatively this is achieved by setting \code{usehistory = TRUE}}
##'   \item{"\strong{acheck}" }{ELink lists all links available for a set of UIDs.}
##'   \item{"\strong{ncheck}" }{ELink checks for the existence of links
##'   \emph{within the same database} for a set of UIDs.}
##'   \item{"\strong{lcheck}" }{Elink checks for the existence of external links
##'   (LinkOuts) for a set of UIDs.}
##'   \item{"\strong{llinks}" }{For each input UID, ELink lists the URLs and
##'   attributes for the LinkOut providers that are not libraries.}
##'   \item{"\strong{llinkslib}" }{For each input UID, ELink lists the URLs and
##'   attributes for all LinkOut providers including libraries.}
##'   \item{"\strong{prlinks}" }{ELink lists the primary LinkOut provider for
##'   each input UID.}
##' }
##' 
##' @param id (Required) A character vector of UIDs.
##' @param dbFrom Initial database containing the UIDs in the input list.
##' @param dbTo Destination database from which to retrieve linked UIDs. If
##' not provided links will be sought in the database containing the input UIDs.
##' @param usehistory If \code{TRUE} search results are stored directly in
##' the user's Web environment so that they can be used in subsequents 
##' calls to \code{\link{esummary}} or \code{\link{efetch}}.
##' @param cmd ELink command mode (default: 'neighbor'). See Details.
##' @param correspondence if \code{TRUE} correspondence between query UIDs and
##' destination UIDs is preserved.
##' @param query_key Query key.
##' @param WebEnv Web Environment.
##' @param linkname Name of the Entrez link to retrieve. Every link in
##' Entrez is given a name of the form 'dbFrom_dbTo_subset'.
##' @param term Search query to limit the output set of linked UIDs.
##' @param holding Name of LinkOut provider.
##' @param datetype Type of date to limit the search. One of 'mdat'
##' (modification date), 'pdat' (publication date) or 'edat' (Entrez date).
##' @param reldate umber of days back for which search items are
##' returned.
##' @param mindate Minimum date of search range. Format YYYY/MM/DD.
##' @param maxdate Maximum date of search range. Format YYYY/MM/DD.
##' 
##' @return An \code{\linkS4class{elink}} object.
##' 
##' @export
##' @example inst/examples/elink.r
elink <- function (id, dbFrom = NULL, dbTo = NULL, usehistory = FALSE,
                   cmd = "neighbor", correspondence = FALSE,
                   query_key = NULL, WebEnv = NULL, linkname = NULL,
                   term = NULL, holding = NULL, datetype = NULL,
                   reldate = NULL, mindate = NULL, maxdate = NULL) {
  
  if (missing(id))
    stop("No UIDs provided")
  
  ## id may be missing if WebEnv and query_key are provided
  if ((is.null(query_key) || is.null(WebEnv)) && missing(id)) {
    stop("No UIDs provided")
  }
  
  ## if WebEnv and query_key are provided, dbFrom must also be provided
  if (!is.null(query_key) && !is.null(WebEnv) && is.null(dbFrom)) {
    stop("No database name provided")
  }
  
  ## construct list of environment variables
  if (missing(id)) {
    ## if WebEnv and query_key is provided by the user set uid=NULL, count=0, 
    ## retmax stays restricted to 500.
    env_list <-list(WebEnv = WebEnv, query_key = query_key, count = 0,
                    uid = NULL, db = dbFrom)
  } else {
    env_list <- .getId(id)
    ## abort if no dbFrom was provided and id did not contain dbFrom 
    if (is.null(dbFrom) && is.null(dbFrom <- env_list$db)) {
      stop("Provide the database containing the input UIDs (dbFrom)")
    }
  }
  
  ## set dbTo = dbFrom if no dbTo is provided
  if (is.null(dbTo) && !grepl(pattern="check$|links", cmd))
    dbTo <- dbFrom
  
  if (usehistory)
    cmd <- "neighbor_history"
  
  if  (correspondence && !is.null(env_list$uid)) {
    id  <- paste0(env_list$uid, collapse="&id=")
  } else {
    id <- .collapse(env_list$uid)
  }

  o <-if (length(env_list$uid) > 100) {
    # use HTTP POST if dealing with more than 100 user provided UIDs.
    .httpPOST('elink', id = id, db = dbTo, dbFrom = dbFrom, cmd = cmd,
              query_key = env_list$query_key, WebEnv = env_list$WebEnv,
              linkname = linkname, term = term, holding = holding,
              datetype = datetype, reldate = reldate, mindate = mindate,
              maxdate = maxdate)
  } else {
   .query('elink', id = id, db = dbTo, dbFrom = dbFrom, cmd = cmd,
           query_key = env_list$query_key, WebEnv = env_list$WebEnv,
           linkname = linkname, term = term, holding = holding,
           datetype = datetype, reldate = reldate, mindate = mindate,
           maxdate = maxdate)
  }
  
  queryKey <- if (length(qk <- xpathSApply(o@content, "//QueryKey")) > 0L) {
    as.integer(xmlValue(qk[[1L]]))
  } else {
    NA_integer_
  }
  
  webEnv <- if (length(we <- xpathSApply(o@content, "//WebEnv")) > 0L) {
    xmlValue(we[[1L]])
  } else {
    NA_character_
  }
      
  if (cmd == "acheck") {
    uid <- xpathSApply(xmlRoot(o@content), "//Id", xmlValue)
    linkset <- .parseIdLinkSet(content=o@content)
  } else if (cmd %in% c("ncheck","lcheck")) {
    uid <- NA_character_
    linkset <- .parseIdCheckList(content=o@content)
  } else if (cmd %in% c("llinks","llinkslib","prlinks")) {
    uid <- xpathSApply(xmlRoot(o@content), "//IdUrlSet/Id", xmlValue)
    linkset <- .parseIdUrlList(content=o@content)
  } else {
    uid <- sapply(getNodeSet(o@content, "//IdList/Id"), xmlValue)
    linkset <- .parseLinkSet(content=o@content)
  }
  
  id <- if (!is.null(env_list$query_key)) {
    .webenv(database = dbFrom, queryKey = env_list$query_key, webEnv = env_list$WebEnv,
            count = env_list$count)
  } else {
    .idlist(database = dbFrom, uid = env_list$uid, count = env_list$count)
  }
  
  el <- .elink(url = o@url, content = o@content, error = checkErrors(o),
               id = id, databaseTo = if (is.null(dbTo)) "any" else dbTo,
               command = cmd, queryKey = queryKey, webEnv = webEnv,
               linkset = linkset)
  el
}




