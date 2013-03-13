#' @autoImports
.equery <- function (eutil, method = "GET", ...) {
  
  method <- match.arg(method, c("GET", "POST"))
  
  if (eutil == "egquery") {
    host <- paste0('http://eutils.ncbi.nlm.nih.gov/entrez/gquery')
  } else {
    host <- paste0('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/', eutil,'.fcgi')
  }
  
  params <- list(...)
  params <- compact(merge_list(params, list(tool = "rentrez", email = "gschofl@yahoo.de")))
  opts <- list()
  hg <- basicHeaderGatherer()
  opts$headerfunction <- hg$update
  tg <- basicTextGatherer()
  opts$writefunction <- tg$update
  
  if (method == "POST") {
    url <- "HTTP_POST"
    postForm(uri=host, .params=params, .opts=opts)
  } else if (method == "GET") {
    url <- get_equery_url(host, params)
    getURLContent(url, .opts=opts)
  }
  
  content <- as.character(tg$value())
  error <- structure(list(error = NULL, errmsg = NULL, wrnmsg = NULL),
                     class="eutil_error")
  header <- as.list(hg$value())
  status <- as.numeric(header$status)
  statusmsg <- header$statusMessage
  if (status != 200) {
    error$error <- paste0("HTTP error: Status ", status, "; ", statusmsg)
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
  
  e <- structure(list(error=error, errmsg=errmsg, wrnmsg=wrnmsg), class="eutil_error")
  invisible(e)
}


#' @autoImports
.getId <- function (id) {
  if (isS4(id)) {
    if (class(id) %in% c("epost", "esearch", "idList")) {
      if (has_webenv(id)) {
        WebEnv <- webEnv(id)
        query_key <- queryKey(id)
        count <- count(id) # the total number of UIDs stored on the history server
        uid <- NULL
        db <- database(id)
      } else {
        WebEnv <- NULL
        query_key <- NULL
        count <- retmax(id) # retmax is the number of UIDs included in the XML output
        uid <- idList(id, db = FALSE)
        db <- database(id)
      }
      return( list(WebEnv=WebEnv, query_key=query_key, count=count, uid=uid, db=db) )
    }   
    if (is(id, "elink")) {
      if (!has_webenv(id)) {
        WebEnv <- NULL
        query_key <- NULL
        count <- length(unlist(linkSet(id), use.names=FALSE))
        uid <- unlist(linkSet(id), use.names=FALSE)
        db <- database(id)[["to"]]
      } else {
        WebEnv <- webEnv(id)
        query_key <- queryKey(id)
        count <- NA # we don't have any clue how many UIDs are stored at the
                    # history server if we use elink with usehistory=TRUE. 
        uid <- NULL
        db <- database(id)[["to"]]
      }
    } else {
      stop("UIDs must be provided as 'esearch', 'epost', or 'elink' objects.")
    }
    return( list(WebEnv=WebEnv, query_key=query_key, count=count, uid=uid, db=db) )
  }
  if (!isS4(id)) {
    # a vector of UIDS as returned by content(esearch_object) 
    if (inherits(id, "character") || inherits(id, "numeric")) {
      WebEnv <- NULL 
      query_key <- NULL
      count <- length(id)
      uid <- as.character(unname(id))
      db <- attr(id, "database")
    } else if (!is.null(names(id))) {
      db <- .convertDbXref(dbx_name=names(id))
    } else {
      stop("UIDs must be provided as a vector of UIDs")
    } 
    return( list(WebEnv=WebEnv, query_key=query_key, count=count, uid=uid, db=db) )
  } 
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


.collapse <- function (id) {
  if (is.null(id)) NULL else paste0(id, collapse = ",")
}


#' @autoImports
has_webenv <- function (x) {
  if (not.na(webEnv(x)) && not.na(queryKey(x)))
    TRUE
  else
    FALSE
}
