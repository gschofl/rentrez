#' @autoImports
.equery <- function (eutil, method = "GET", ...) {
  
  method <- match.arg(method, c("GET", "POST"))
  
  if (eutil == "egquery") {
    host <- paste0('http://eutils.ncbi.nlm.nih.gov/entrez/gquery')
  } else {
    host <- paste0('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/', eutil,'.fcgi')
  }
  
  params <- list(...)
  params <- rmisc::compact(merge_list(params, list(tool = "Rentrez", email = "gschofl@yahoo.de")))
  opts <- list()
  hg <- basicHeaderGatherer()
  opts$headerfunction <- hg$update
  tg <- basicTextGatherer()
  opts$writefunction <- tg$update
  
  if (method == "POST") {
    url <- "HTTP_POST"
    e <- tryCatch(postForm(uri=host, .params=params, .opts=opts), error = function(e) {
      e$message
    })
  } else if (method == "GET") {
    url <- get_equery_url(host, params)
    e <- tryCatch(getURLContent(url, .opts=opts), error = function(e) {
      e$message
    })
  }
  
  content <- as.character(tg$value())
  error <- structure(list(error=NULL, errmsg=NULL, wrnmsg=NULL), class="eutil_error")
  if (all_empty(e)) {
    header <- as.list(hg$value())
    status <- as.numeric(header$status)
    statusmsg <- header$statusMessage
    if (status != 200) {
      error$error <- paste0("HTTP error: Status ", status, "; ", statusmsg)
      warning(error$error, call.=FALSE, immediate.=TRUE)
    }
  } else {
    error$error <- paste0("CurlError: ", e)
    warning(error$error, call.=FALSE, immediate.=TRUE)
  }

  new("eutil", url = url, error = error, content = content)
}


#' @autoImports
get_equery_url <- function (host, params) {
  fields <- paste(curlEscape(names(params)), curlEscape(params), sep="=", collapse="&")
  paste0(host, "?", fields)
}


#' @autoImports
.escape <- function (x) {
  x <- gsub("\\s+", " ", x)
  gsub(" (and) | (or) | (not) "," \\U\\1\\U\\2\\U\\3 ", x, perl=TRUE)
}


#' @autoImports
checkErrors <- function (o, verbose = TRUE) {
  
  if (is(o, "eutil")) {
    response <- content(o, 'xml')
  } else if (is(o, "XMLInternalDocument")) {
    response <- o
  }
  
  error <- xvalue(response, '//ERROR', NULL) 
  errmsg_name <- xname(response, '//ErrorList/*', NULL)
  errmsg <- setNames(xvalue(response, '//ErrorList/*', NULL), nm=errmsg_name)
  wrnmsg_name <- xname(response, '//WarningList/*', NULL)
  wrnmsg <- setNames(xvalue(response, '//WarningList/*', NULL), nm=wrnmsg_name) 
   
  if (!all_empty(error) && verbose)
    message('Error:\n\t', error)
  
  if (!all_empty(errmsg) && verbose)
    message('Error(s):\n\t', 
            paste(paste(names(errmsg), errmsg, sep="\t"), collapse="\n\t"))
  
  if (!all_empty(wrnmsg) && verbose)
    message('Warning(s):\n\t', 
            paste(paste(names(wrnmsg), wrnmsg, sep="\t"), collapse="\n\t"))
  
  invisible(structure(list(error=error, errmsg=errmsg, wrnmsg=wrnmsg),
                      class="eutil_error"))
}


#' @autoImports
savelyParseXml <- function(x, ...) {
  tryCatch(xmlTreeParse(x, asText=TRUE, useInternalNodes=TRUE,
                        error=NULL, ...),
           "XMLError" = function (e) {
             errmsg <- paste("XML parse error:", e$message)
             xmlParseString(paste0("<ERROR>", errmsg, "</ERROR>"))
           },
           "error" = function (e) {
             errmsg <- paste("Simple error:", e$message)
             xmlParseString(paste0("<ERROR>", errmsg, "</ERROR>"))
           })
}


#' @autoImports
.get_params <- function (id) {
  if (isS4(id)) {
    if (class(id) %in% c("epost", "esearch", "idList")) {
      if (has_webenv(id)) {
        db <- database(id)
        WebEnv <- webEnv(id)
        query_key <- queryKey(id)
        count <- count(id) # the total number of UIDs stored on the history server
        uid <- NULL 
      } else {
        db <- database(id)
        WebEnv <- NULL
        query_key <- NULL
        count <- retmax(id) # retmax is the number of UIDs included in the XML output
        uid <- idList(id, db = FALSE)
      }
      return( list(db=db, WebEnv=WebEnv, query_key=query_key, count=count, uid=uid) )
    }   
    if (is(id, "elink")) {
      if (!has_webenv(id)) {
        db <- database(id)[["to"]]
        WebEnv <- NULL
        query_key <- NULL
        count <- length(unlist(linkSet(id), use.names=FALSE))
        uid <- unlist(linkSet(id), use.names=FALSE)
      } else {
        db <- database(id)[["to"]]
        WebEnv <- webEnv(id)
        query_key <- queryKey(id)
        count <- NA # we don't have any clue how many UIDs are stored at the
        # history server if we use elink with usehistory=TRUE. 
        uid <- NULL
      }
    } else {
      stop("UIDs must be provided as 'esearch', 'epost', or 'elink' objects.")
    }
    return( list(db=db, WebEnv=WebEnv, query_key=query_key, count=count, uid=uid) )
  }
  if (!isS4(id)) {
    # a vector of UIDS as returned by content(esearch_object) 
    if (inherits(id, "character") || inherits(id, "numeric")) {
      db <- attr(id, "database")
      WebEnv <- NULL 
      query_key <- NULL
      count <- length(id)
      uid <- as.character(unname(id))
    } else if (!is.null(names(id))) {
      db <- .convertDbXref(dbx_name=names(id))
    } else {
      stop("UIDs must be provided as a vector of UIDs")
    } 
    return( list(db=db, WebEnv=WebEnv, query_key=query_key, count=count, uid=uid) )
  } 
}


#' @autoImports
get_params <- function (id, db = NULL, WebEnv = NULL, query_key = NULL) {
  
  ## id may be missing only if WebEnv and query_key are provided
  if ( missing(id) && (is.null(query_key) || is.null(WebEnv)) ) {
    stop("No UIDs provided", call.=FALSE)
  }
  
  ## if WebEnv and query_key are provided, db must also be provided
  if (!is.null(query_key) && !is.null(WebEnv) && is.null(db)) {
    stop("No database name provided", call.=FALSE)
  }
  
  if (missing(id)) {
    ## if WebEnv and query_key is provided by the user set uid=NULL, count=0, 
    ## rerestricted to 500.
    params <- list(db = db, WebEnv = WebEnv, query_key = query_key,
                   count = 0, uid = NULL)
  } else {
    params <- .get_params(id)
    
    ## abort if id did not contain db 
    params$db <- db %|null|% params$db
    if (is.null(params$db))
      stop("No database name provided", call.=FALSE)
  }
  
  return( params )
}


#' @autoImports
.convertDbXref <- function (dbx_name) {
  if (length(dbx_name) > 1L) {
    stop("Multiple database names. Provide only one.")
  }
  match <- regexpr("[^\\.][[:alpha:]]+$", dbx_name)
  if (match > 0L) {
    dbx_name <- regmatches(dbx_name, match)
  }
  dbx_name <- switch(dbx_name,
                     GI = "nuccore",
                     GeneID = "gene",
                     taxon = "taxonomy",
                     CDD = "cdd",
                     biosample = "biosample",
                     NULL)
  dbx_name
}


#' @autoImports
.collapse <- function (id) {
  if (is.null(id)) NULL else paste0(id, collapse = ",")
}


#' @autoImports
has_webenv <- function (x) {
  !is.na(webEnv(x)) && !is.na(queryKey(x))
}

