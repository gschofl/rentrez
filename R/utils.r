#' @autoImports
.query <- function (eutil, ...) {
  
  if (identical(eutil, "egquery")) {
    url <- get_query_url('http://eutils.ncbi.nlm.nih.gov/gquery/',
                         ..., , tool="rentrez", email="gschofl@yahoo.de")
  } else {
    host <- paste0('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/', eutil, '.fcgi')
    url <- get_query_url(host, ..., tool="rentrez", email="gschofl@yahoo.de")
  }

  if (identical(eutil, "efetch")) {
    .eutil(url = curlUnescape(url), content = getURL(url))
  } else {
    .eutil(url = curlUnescape(url), content = xmlTreeParse(getURL(url), useInternalNodes=TRUE))
  }
}


#' @autoImports
get_query_url <- function (host, ...) {
  if (missing(host)) {
    stop("No host url provided")
  }
  args <- compact(list(...))
  params <- names(args)
  fields <- sprintf("%s=%s", as.character(params),
                    vapply(args, paste0, collapse=",",
                           FUN.VALUE=character(1), USE.NAMES=FALSE))
  sprintf('%s%s', host, .escape(paste0("?", paste0(fields, collapse="&"))))
}


#' @autoImports
.escape <- function (s, httpPOST=FALSE) {
  if (httpPOST) {
    s <- gsub("\\s+", " ", s)
    s <- gsub("+", " ", s, fixed=TRUE)
  } else {
    s <- gsub("\\s+", "\\+", s)
  }
  s <- paste(strsplit(s, '\"', fixed=TRUE)[[1L]], collapse="%22")
  s <- gsub(">", "%3E", s)
  s <- gsub("\\n", "%0D%0A", s)
  s <- gsub("\\|", "%7C", s)
  s <- gsub("\\#", "%23", s)
  s <- gsub("\\+(and)\\+|\\+(or)\\+|\\+(not)\\+","\\+\\U\\1\\U\\2\\U\\3\\+", s, perl=TRUE)
  s
}


#' @autoImports
.httpPOST <- function (eutil, ...) {
  
  user_agent <- switch(eutil,
                       elink='elink/1.0',
                       esearch='esearch/1.0',
                       epost='epost/1.0',
                       esummary='esummary/1.0',
                       efetch='efetch/1.0')
  
  http_post_url <- switch(eutil,
                          elink='elink.fcgi?',
                          esearch='esearch.fcgi?',
                          epost='epost.fcgi?',
                          esummary='esummary.fcgi?',
                          efetch='efetch.fcgi?')
  
  doc <- postForm(paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils',
                        http_post_url, sep="/"),
                  ..., type='POST', .opts=curlOptions(useragent=user_agent))
  
  if (identical(eutil, "efetch")) {
    .eutil(url = 'HTTP_POST', content = as.character(doc))
  } else {
    .eutil(url = 'HTTP_POST', content = xmlTreeParse(doc, useInternalNodes=TRUE))
  }
}


#' @autoImports
checkErrors <- function (o) {
  error <- NULL
  err_msgs <- NULL
  wrn_msgs <- NULL
  
  err_node <- getNodeSet(o@content, '//ERROR')
  if (length(err_node) > 0)
    error <- lapply(err_node, xmlValue)
  
  err_list_node <- getNodeSet(o@content, '//ErrorList')
  if (length(err_list_node) > 0)
    err_msgs <- lapply(xmlChildren(err_list_node[[1]]), xmlValue)
  
  wrn_list_node <- getNodeSet(o@content, '//WarningList')
  if (length(wrn_list_node) > 0)
    wrn_msgs <- lapply(xmlChildren(wrn_list_node[[1]]), xmlValue)
  
  if (!is.null(error))
    message('Error:\n\t', unlist(error))
  
  if (!is.null(err_msgs))
    message('Error(s):\n\t', 
            paste(paste(names(err_msgs), err_msgs, sep="\t"), collapse="\n\t"))
  
  if (!is.null(wrn_msgs))
    message('Warning(s):\n\t', 
            paste(paste(names(wrn_msgs), wrn_msgs, sep="\t"), collapse="\n\t"))
  
  invisible(list(err=error, errmsg=err_msgs, wrnmsg=wrn_msgs))
}


#' @autoImports
.getId <- function (id) {
  
  if (isS4(id)) {
    if (class(id) %in% c("esearch","idlist","webenv")) {
      if (is(id, "esearch")) {
        id <- id@id
      }
      if (is(id, "idlist")) {
        WebEnv <- NULL
        query_key <- NULL
        count <- id@retMax # retMax is the number of UIDs included in the XML output
        uid <- id@uid
        db <- id@database
      } else if (is(id, "webenv")) {
        WebEnv <- id@webEnv
        query_key <- id@queryKey
        count <- id@count # the total number of UIDs stored on the history server
        uid <- NULL
        db <- id@database
      }
      return( list(WebEnv=WebEnv, query_key=query_key, count=count, uid=uid, db=db) )
    }
    
    if (is(id, "epost")) {
      WebEnv <- id@webEnv
      query_key <- id@queryKey
      count <- id@count
      uid <- NULL
      db <- id@database
    } else if (is(id, "elink")) {
      if (is.na(id@queryKey) && is.na(id@webEnv)) {
        WebEnv <- NULL
        query_key <- NULL
        count <- length(unlist(id@linkset))
        uid <- unname(unlist(id@linkset))
        db <- id@databaseTo
      } else {
        WebEnv <- id@webEnv
        query_key <- id@queryKey
        count <- NA # we don't have any clue how many UIDs are stored at the
        # history server if we use elink with usehistory=TRUE. 
        uid <- NULL
        db <- id@databaseTo
      }
    } else {
      stop("UIDs must be provided as 'esearch', 'epost', or 'elink' objects.")
    }
    
    return( list(WebEnv=WebEnv, query_key=query_key, count=count, uid=uid, db=db) )
    
  }
  
  if (!isS4(id)) {
    ## a vector of UIDS as returned by content(esearch_object) 
    if (is(id, "idlist") || inherits(id, "character") || inherits(id, "numeric")) {
      WebEnv <- NULL 
      query_key <- NULL
      count <- length(id)
      uid <- as.character(unname(id))
      db <- attr(id, "database")
      ## a list as returned by content(epost_object)
    } else if (is(id, "webenv")) {
      WebEnv <- id$webEnv
      query_key <- id$queryKey
      count <- NA_integer_
      uid <- NULL
      db <- attr(id, "database")
    } else if (!is.null(names(id))) {
      db <- .convertDbXref(names(id))
    } else {
      stop("UIDs must be provided as a vector of UIDs or as a 'webenv' object")
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

