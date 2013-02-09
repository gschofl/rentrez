#' @include utils.r
#' @include eutil.r
NULL


# elink-class ------------------------------------------------------------


#' elink
#' 
#' \dQuote{elink} is an S4 class that provides a container for data retrived
#' by calls to the NCBI ELink utility.
#' 
#' @slot url A character vector containing the query URL.
#' @slot error Any error or warning messages parsed from
#' the output of the call submitted to Entrez.
#' @slot content A \code{\linkS4class{raw}} vector holding the unparsed
#' contents of a request to Entrez.
#' @slot idList
#' @slot databaseTo 
#' @slot command 
#' @slot queryKey 
#' @slot webEnv 
#' @slot linkSet A list containing the linked data.
#' 
#' @rdname elink
#' @export
#' @classHierarchy
#' @classMethods
setClass("elink",
         representation(idList = "idList",
                        databaseTo = "character",
                        command = "character",
                        queryKey = "integer",
                        webEnv = "character",
                        linkSet = "list"),
         prototype(idList = new("idList"),
                   databaseTo = NA_character_,
                   command = NA_character_,
                   queryKey = NA_integer_,
                   webEnv = NA_character_,
                   linkSet = list()),
         contains = "eutil")


# accessor-methods -------------------------------------------------------


setMethod("database", "elink", function(x) c(from = database(x@idList),
                                             to = x@databaseTo))

setMethod("count", "elink", function(x) count(x@idList))

setMethod("idList", "elink", function(x) x@idList)

setMethod("queryKey", "elink", function(x) x@queryKey)

setMethod("webEnv", "elink", function(x) x@webEnv)

setMethod("linkSet", "elink", function(x) x@linkSet)

setMethod("content", "elink", function(x, as = "xml") {
  callNextMethod(x = x, as = as)
})


# subsetting-method ------------------------------------------------------


setMethod("[", c("elink", "ANY", "missing"),
          function (x, i, j, ..., drop = TRUE) {
            ids <- unlist(x@linkSet[i][[1L]][["id"]], use.names=FALSE)
            new("idList", database = database(x)[["to"]],
                idList = ids, count = length(ids))
          })


# show-method ------------------------------------------------------------


setMethod("show", "elink",
          function (object) {
            if (object@command == "acheck")
              .show.acheck(object)
            else if (object@command == "ncheck")
              .show.ncheck(object)
            else if (object@command == "lcheck")
              .show.lcheck(object)
            else if (object@command %in% c("llinks","llinkslib","prlinks"))
              .show.links(object)
            else {
              cat(sprintf("ELink from database %s to database %s.\n",
                          sQuote(database(object)[["from"]]),
                          sQuote(database(object)[["to"]])))
              
              if (has_webenv(object)) {
                cat(sprintf("Query Key: %s\nWeb Environment: %s\n",
                            queryKey(object@idList),
                            webEnv(object@idList)))
              } else {            
                cat("IdList:\n")
                print(idList(object@idList))
              }
              
              cat("Summary of LinkSet:\n")
              lnames <- names(object@linkSet)
              llen <- numeric(0)
              for (lname in lnames)
                llen <- c(llen, length(object@linkSet[[lname]][["id"]]))
              print(data.frame(LinkName=lnames, LinkCount=llen))
              
              invisible()
            }
          })


.show.acheck <- function (object) {
  cat("ELink list of possible links for a set of UIDs:\n")
  print(object@linkSet)
  invisible()
}


.show.ncheck <- function (object) {
  cat("Existence of links within the same database for a set of UIDs\n")
  print(object@linkSet)
  invisible()
}


.show.lcheck <- function (object) {
  cat("Existence of external links for a set of UIDs\n")
  print(object@linkSet)
  invisible()
}


#' @autoImports
.show.links <- function (object) {
  cat("External links for UID:")
  w <- getOption("width")
  x <- lapply(object@linkSet, function(o) {
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


#' \code{elink} generates a list of UIDs in a specified Entrez database that
#' are linked to a set of input UIDs in either the same or another
#' database. For instance, the ELink utility can find Entrez gene records
#' linked to records in Entrez Protein.
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ELink}{EUtilities}
#' for additional information.
#' 
#' If \code{dbTo} and \code{dbFrom} are set to the same database, ELink will
#' return neighbors within that database.
#' 
#' Elink commands (cmd) specify the function that elink will perform.
#' Available commands are:
#' \itemize{
#'   \item{"\strong{neighbor}" }{(Default) ELink returns a set of UIDs in dbTo
#'   linked to the input UIDs in dbFrom.}
#'   \item{"\strong{neighbor_score}" }{ELink returns a set of UIDs within the
#'   same database as the input UIDs along with similarity scores.}
#'   \item{"\strong{neighbor_history}" }{ELink posts the output UIDs to the
#'   Entrez History server and returns a query_key and WebEnv parameter.
#'   Alternatively this is achieved by setting \code{usehistory = TRUE}}
#'   \item{"\strong{acheck}" }{ELink lists all links available for a set of UIDs.}
#'   \item{"\strong{ncheck}" }{ELink checks for the existence of links
#'   \emph{within the same database} for a set of UIDs.}
#'   \item{"\strong{lcheck}" }{Elink checks for the existence of external links
#'   (LinkOuts) for a set of UIDs.}
#'   \item{"\strong{llinks}" }{For each input UID, ELink lists the URLs and
#'   attributes for the LinkOut providers that are not libraries.}
#'   \item{"\strong{llinkslib}" }{For each input UID, ELink lists the URLs and
#'   attributes for all LinkOut providers including libraries.}
#'   \item{"\strong{prlinks}" }{ELink lists the primary LinkOut provider for
#'   each input UID.}
#' }
#' 
#' @param id (Required) A character vector of UIDs.
#' @param dbFrom Initial database containing the UIDs in the input list.
#' @param dbTo Destination database from which to retrieve linked UIDs. If
#' not provided links will be sought in the database containing the input UIDs.
#' @param linkname Name of the Entrez link to retrieve. Every link in
#' Entrez is given a name of the form 'dbFrom_dbTo_subset'.
#' @param usehistory If \code{TRUE} search results are stored directly in
#' the user's Web environment so that they can be used in subsequents 
#' calls to \code{\link{esummary}} or \code{\link{efetch}}.
#' @param cmd ELink command mode (default: 'neighbor'). See Details.
#' @param correspondence if \code{TRUE} correspondence between query UIDs and
#' destination UIDs is preserved.
#' @param query_key Query key.
#' @param WebEnv Web Environment.
#' @param term Search query to limit the output set of linked UIDs.
#' @param holding Name of LinkOut provider.
#' @param datetype Type of date to limit the search. One of 'mdat'
#' (modification date), 'pdat' (publication date) or 'edat' (Entrez date).
#' @param reldate umber of days back for which search items are
#' returned.
#' @param mindate Minimum date of search range. Format YYYY/MM/DD.
#' @param maxdate Maximum date of search range. Format YYYY/MM/DD.
#' @return An \code{elink} instance.
#' @export
#' @example inst/examples/elink.r
#' @autoImports
elink <- function (id, dbFrom = NULL, dbTo = NULL, linkname = NULL,
                   usehistory = FALSE, cmd = "neighbor",
                   correspondence = FALSE, query_key = NULL, WebEnv = NULL,
                   term = NULL, holding = NULL, datetype = NULL,
                   reldate = NULL, mindate = NULL, maxdate = NULL) {
  
  if (missing(id))
    stop("No UIDs provided")
  
  # id may be missing if WebEnv and query_key are provided
  if ((is.null(query_key) || is.null(WebEnv)) && missing(id)) {
    stop("No UIDs provided")
  }
  
  # if WebEnv and query_key are provided, dbFrom must also be provided
  if (not.null(query_key) && not.null(WebEnv) && is.null(dbFrom)) {
    stop("No database name provided")
  }
  
  # construct list of environment variables
  if (missing(id)) {
    # if WebEnv and query_key is provided by the user set uid=NULL, count=0, 
    # retmax stays restricted to 500.
    env_list <- list(WebEnv = WebEnv, query_key = query_key, count = 0,
                     uid = NULL, db = dbFrom)
  } else {
    env_list <- .getId(id)
    
    # abort if no dbFrom was provided and id did not contain dbFrom 
    dbFrom <- dbFrom %|null|% env_list$db
    if (is.null(dbFrom))
      stop("Provide the database containing the input UIDs (dbFrom)")
    
  }
  
  # set dbTo = dbFrom if no dbTo is provided
  if (is.null(dbTo) && !grepl(pattern="check$|links", cmd))
    dbTo <- dbFrom
  
  if (usehistory)
    cmd <- "neighbor_history"
  
  if  (correspondence && not.null(env_list$uid)) {
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
  
  response <- content(o, "xml")
  queryKey <- xvalue(response, '//QueryKey', as='integer')
  webEnv <- xvalue(response, '//WebEnv')
      
  if (cmd == "acheck") {
    uid <- xvalue(response, "//Id")
    linkSet <- .parseIdLinkSet(response)
  } else if (cmd %in% c("ncheck","lcheck")) {
    uid <- NA_character_
    linkSet <- .parseIdCheckList(response)
  } else if (cmd %in% c("llinks","llinkslib","prlinks")) {
    uid <- xvalue(response, "//IdUrlSet/Id")
    linkSet <- .parseIdUrlList(response)
  } else {
    uid <- xvalue(response, "//IdList/Id")
    linkSet <- .parseLinkSet(response)
  }
  
  
  id <- new("idList", database = dbFrom, count = length(uid),
             queryKey = env_list$query_key %|null|% NA_integer_,
             webEnv = env_list$WebEnv %|null|% NA_character_,
             idList = uid)
  
  new("elink", url = queryUrl(o), content = content(o, "raw"),
      error = checkErrors(o), idList = id,
      databaseTo = if (is.null(dbTo)) "any" else dbTo,
      command = cmd, queryKey = queryKey, webEnv = webEnv,
      linkSet = linkSet)
}




