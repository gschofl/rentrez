#' Construct url, fetch response, construct eutil object
#' 
#' @importFrom RCurl curlUnescape
#' @importFrom RCurl getURL
#' @keywords internal
.query <- function (eutil, ...)
{
  eutils_host <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/'
  #query_string <- .query_string(...)
  query_string <- .query_string(..., tool="Rentrez", email="gschofl@yahoo.de")
  
  if (identical(eutil, "egquery"))
    url <- sprintf('http://eutils.ncbi.nlm.nih.gov/gquery/%s', query_string)
  else
    url <- sprintf('%s%s.fcgi%s', eutils_host, eutil, query_string)
  
  if (identical(eutil, "efetch"))
    new("eutil",
        url=curlUnescape(url),
        data=getURL(url))
  else
    new("eutil",
        url=curlUnescape(url),
        data=xmlTreeParse(getURL(url), useInternalNodes=TRUE))
}

.query_string <- function (...)
{
  args <- list(...)
  params <- names(args)
  empty <- sapply(args, is.null)
  fields <- paste(as.character(params[!empty]), as.character(args[!empty]), sep="=")
  .escape(paste("?", paste(fields, collapse="&"), sep=""))
}

.escape <- function (s, httpPOST=FALSE)
{
  if (httpPOST) {
    s <- gsub(" +", " ", s)
    s <- gsub("+", " ", s, fixed=TRUE)
  }
  else {
    s <- gsub(" +", "\\+", s)
  }
  s <- paste(strsplit(s, '\"', fixed=TRUE)[[1L]], collapse="%22")
  s <- gsub(">", "%3E", s)
  s <- gsub("\\n", "%0D%0A", s)
  s <- gsub("\\|", "%7C", s)
  s <- gsub("\\#", "%23", s)
  s <- gsub("\\+(and)\\+|\\+(or)\\+|\\+(not)\\+","\\+\\U\\1\\U\\2\\U\\3\\+", s, perl=TRUE)
  s
}

#' use HTTP POST
#' 
#' @importFrom RCurl postForm
#' @importFrom RCurl curlOptions
#' @keywords internal
.httpPOST <- function (eutil, ...)
{
  
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
                 http_post_url, sep="/"), ..., type='POST',
                  .opts=curlOptions(useragent=user_agent))
  
  if (identical(eutil, "efetch")) {
    new('eutil', url='HTTP_POST', data=as.character(doc))
  }
  else {
    new('eutil',
        url='HTTP_POST',
        data=xmlTreeParse(doc, useInternalNodes=TRUE))
  }

}

# Parse a DocSum recursively and return it as a named list
.parseDocSum <- function (ds)
{
  if (xmlName(ds) == "DocSum") {
    .docsum <- function (ds) {
      items <- 
        xmlChildren(ds, addNames=FALSE)[names(xmlChildren(ds)) == "Item"]      
      value <- 
        lapply(items, function (item) {
          if (all(xmlSApply(item, xmlSize) == 0L))
            xmlValue(item)
          else
            .docsum(item)
        })
      names(value) <- 
        lapply(items, function (item) xmlGetAttr(item, "Name"))
      return(value)
    }
    return(.docsum(ds))
  }
  else if (xmlName(ds) == "DocumentSummary") {
    .docsum <- function (ds) {
      items <- 
        xmlChildren(ds, addNames=TRUE)
      value <- 
        lapply(items, function (item) {
          if (all(xmlSApply(item, xmlSize) == 0L))
            xmlValue(item)
          else
            .docsum(item)
        })
      names(value) <- lapply(items, xmlName)
      return(value)
    }
    return(.docsum(ds))
  }
}

# Parse IdCheckList returned from cmd=ncheck
.parseIdCheckList <- function (data=o@data)
{
  data <- xmlRoot(data)
  dbFrom <- xpathSApply(data, "//DbFrom", xmlValue)
  id <- xpathSApply(data, "//Id", xmlValue)
  has_neighbor <- xpathSApply(data, "//Id", xmlGetAttr, "HasNeighbor")
  
  chklst <- data.frame(stringsAsFactors=FALSE, Id=id,
                       HasNeighbor=ifelse(has_neighbor == "Y", TRUE, FALSE))
  chklst
}

# Parse a LinkSet and return it as a data.frame
.parseIdLinkSet <- function (data)
{
  data <- xmlRoot(data)
  dbFrom <- xpathSApply(data, "//DbFrom", xmlValue)
  idLinkSet <- getNodeSet(xmlRoot(data), "//IdLinkSet")
  
  if (length(idLinkSet) < 1L)
    return(list())
  
  ll <- lapply(idLinkSet, function (ls) {
    ls <- xmlDoc(ls)
    Id <- xpathSApply(ls, "(//Id)[1]", xmlValue)
    link_info <- 
      lapply(getNodeSet(ls, "//LinkInfo"), function (li) {
        li <- xmlDoc(li)
        li <- list(DbTo=xpathSApply(li, "//DbTo", xmlValue), 
                   LinkName=xpathSApply(li, "//LinkName", xmlValue),
                   MenuTag=xpathSApply(li, "//MenuTag", xmlValue),
                   HtmlTag=xpathSApply(li, "//HtmlTag", xmlValue),
                   Priority=xpathSApply(li, "//Priority", xmlValue))
        li[vapply(li, length, integer(1)) == 0L] <- NA_character_
        li
      })
    data.frame(stringsAsFactors=FALSE, Id=Id, 
               do.call(rbind, link_info))
  })
  
  ll
}

# Parse a LinkSet and return it as a named list
.parseLinkSet <- function (data)
{
  linkSetDb <- getNodeSet(xmlRoot(data), "//LinkSetDb")
  
  if (length(linkSetDb) < 1L)
    return(list())
  
  ll <- lapply(linkSetDb, function(lsd) {
    lsd <- xmlDoc(lsd)
    id <- xpathSApply(lsd, "//Id", xmlValue)
    score <- xpathSApply(lsd, "//Score", xmlValue)
    ans <- list(id=id, score=score)
    ans[vapply(ans, length, integer(1)) == 0L]  <- NULL
    ans
  })
  
  names(ll) <- xpathSApply(xmlRoot(data), "//LinkName", xmlValue)
  ll
}

checkErrors <- function (obj)
{
  error <- NULL
  err_msgs <- NULL
  wrn_msgs <- NULL
  
  err_node <- getNodeSet(slot(obj, "data"), '//ERROR')
  if (length(err_node) > 0)
    error <- lapply(err_node, xmlValue)
  
  err_list_node <- getNodeSet(slot(obj, "data"), '//ErrorList')
  if (length(err_list_node) > 0)
    err_msgs <- lapply(xmlChildren(err_list_node[[1]]), xmlValue)
  
  wrn_list_node <- getNodeSet(slot(obj, "data"), '//WarningList')
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
  
  return(invisible(list(err=error, errmsg=err_msgs, wrnmsg=wrn_msgs)))
}

.getDb <- function (object)
{
  if (is(object, "esearch") || is(object, "epost") || is(object, "idlist"))
    db <- object@database
  else if (is(object, "elink"))
    db <- object@databaseTo
  else
    db <- NULL
  db
}

.getId <- function (object)
{
  # we need the count basically for efetch.batch
  if (is(object, "epost")) {
    WebEnv <- object@webEnv
    query_key <- object@queryKey
    count <- object@count
    id <- NULL
  }
  else if (is(object, "elink")) {
    if (is.na(object@queryKey) && is.na(object@webEnv)) {
      WebEnv <- NULL
      query_key <- NULL
      count <- length(unlist(object@linkList))
      id <- unlist(object@linkList)
    } else {
      WebEnv <- object@webEnv
      query_key <- object@queryKey
      count <- NA # we don't have any clue how many UIDs are stored at the
                 # history server if we use elink with usehistory=TRUE. 
      id <- NULL
    }
  }
  else if (is(object, "esearch") || is(object, "idlist")) {
    if (is.na(object@queryKey) && is.na(object@webEnv)) {
      WebEnv <- NULL
      query_key <- NULL
      count <- length(object@idList)
      id <- object@idList
    } else {
      WebEnv <- object@webEnv
      query_key <- object@queryKey
      count <- object@count
      id <- NULL
    }
  }
  else if (is.atomic(object)) {
    WebEnv <- NULL 
    query_key <- NULL
    count <- length(object)
    id <- object
  }
  else
    stop("UIDs must be provided as a vector or as esearch objects.")
  
  return(list(WebEnv=WebEnv, query_key=query_key, count=count, id=id))
}

isEmpty <- function (x) length(x) == 0L

.collapse <- function (id) paste0(id, collapse = ",")

#' Flatten (Nested) Lists.
#'
#' Flatten \code{lists} according to specifications made via
#' \code{start_after} and/or \code{stop_at}. When keeping 
#' the defaults, the function will traverse \code{src} to retrieve the
#' values at the respective bottom layers/bottom elements. These values are
#' arranged in a named \code{list} where the respective names can be
#' interpreted as the the paths to the retrieved values.   
#'
#' @param x An arbitrarily deeply nested \code{list}
#' @param start_after An \code{integer} specifying the layer after which to 
#' start the flattening. \code{NULL} means to start at the very top.
#' @param stop_at An \code{integer} specifying the layer at which to stop
#' the flattening. \code{NULL} means there is not stop criterion.
#' @param delim_path A \code{character} specifying how the names
#' of the resulting flattened list should be pasted.
#' @param ... Further args.
#' @return A named \code{list} that features the desired degree of flattening.
#' @keywords internal
#' @author Janko Thyson \email{janko.thyson.rstuff@@googlemail.com}
#' @examples
#'  ##
flatten <- function (x, 
                     start_after=NULL, 
                     stop_at=NULL, 
                     delim_path=".",
                     do_warn=TRUE,
                     ... )
{
  # VALIDATE
  if (!is.list(x)) {
    stop("'src' must be a list.")
  }
  if (!is.null(start_after) && !is.null(stop_at)) {
    if (start_after == 1 && stop_at == 1)
      stop(sprintf("Invalid specification:\nstart_after: %s\nstop_at: %s\n",
                   start_after, stop_at))
  }

  # INNER FUNCTIONS
  .startAfterInner <- function(envir, nms, out.1, ...)
  {
    idx_diff <- diff(c(envir$start_after, length(envir$counter)))

    # UPDATE IF DEGREE OF NESTEDNESS EXCEEDS START CRITERION
    if (idx_diff > 0) {
      idx_cutoff <-
        seq(from=(length(envir$counter) - idx_diff + 1), to=length(envir$counter))
      
      idx_left        <- envir$counter[-idx_cutoff]
      nms.1           <- nms[idx_cutoff]
      names(out.1)    <- paste(nms.1, collapse=envir$delim_path)
      # UPDATE SRC
      idx_append <- sapply(envir$history, function (x_hist) {
        all(idx_left == x_hist)        
      })

      if (any(idx_append)) {                                          
        envir$src[[idx_left]] <- append(envir$src[[idx_left]], values=out.1)                    
      }
      else {
        envir$src[[idx_left]] <- out.1
        # UPDATE HISTORY
        envir$history <- c(envir$history, list(idx_left))
      }
      envir$out <- envir$src          
    } 
    else if (idx_diff < 0) {
      envir$out <- envir$src
    }
    
    # RESET
    envir$nms <- envir$nms[-length(envir$nms)]
    envir$counter <- envir$counter[-length(envir$counter)]
    
    return(TRUE)
  }
  
  .updateOutInner <- function (envir, out.1, ...)
  {

    # UPDATE OUT
    envir$out <- c(get("out", envir = envir), out.1)

    # RESET
    envir$nms       <- envir$nms[-length(envir$nms)]
    envir$counter   <- envir$counter[-length(envir$counter)]
    
    return(TRUE)
  }
  
  .flattenInner <- function(x, envir, ...)
  {
    if ( is(x, "list") && length(x) != 0 ) {
      
      # UPDATE
      envir$counter_history <- c(envir$counter_history, list(envir$counter))
      
      # EXIT IF DEGREE EXCEEDS CUTOFF
      if (!is.null(envir$stop_at)) {
        if (length(envir$counter) > envir$stop_at) { 
          nms <- get("nms", envir=envir)
          out.1 <- list(x)
          names(out.1) <- paste(nms, collapse=envir$delim_path)

          # DECISION ON FLATTENING
          if (!is.null(envir$start_after)) {
            .startAfterInner(envir=envir, nms=nms, out.1=out.1)
            return(NULL)
          }
          else {
            .updateOutInner(envir=envir, out.1=out.1)
            return(NULL)
          }
        }
      }

      # LOOP OVER ELEMENTS
      for (i in seq_along(x)) {
        # UPDATE COUNTER
        envir$counter <- c(envir$counter, i)
        # UPDATE NAMES
        list_names <- if (is.null(names(x[i]))) paste0("X", i) else names(x[i])
        assign("nms", c(get("nms", envir=envir), list_names), envir=envir)
        # RECURSIVE FLATTENING
        .flattenInner(x=x[[i]], envir) # call  recursively
        # RESET COUNTER
        if (i == length(x)) {
          envir$nms <- envir$nms[-length(envir$nms)]
          envir$counter <- envir$counter[-length(envir$counter)]
        }
      }
    } 
    else {

      nms <- get("nms", envir=envir)
      out.1 <- list(x)
      names(out.1) <- paste(nms, collapse=envir$delim_path)

      # DECISION ON FLATTENING
      if (!is.null(envir$start_after))
        .startAfterInner(envir=envir, nms=nms, out.1=out.1)
      else
        .updateOutInner(envir=envir, out.1=out.1)
    }
        
    return(TRUE)
  }
  
  out                     <- list()
  # ENVIR
  envir                   <- new.env()
  envir$counter           <- NULL
  envir$counter_history   <- NULL
  envir$delim_path        <- delim_path
  envir$do_warn           <- do_warn
  envir$do_block_warning  <- FALSE
  envir$history           <- NULL
  envir$nms               <- NULL
  envir$out               <- list()
  envir$src               <- x
  envir$start_after       <- start_after
  
  if (!is.null(stop_at)) {
    stop_at_0 <- stop_at
    if (stop_at == 1) {
      return(src)
    } else {
      stop_at <- stop_at - 1
    }
  }
  
  envir$stop_at           <- stop_at

  .flattenInner(x, envir)
  
  if (envir$do_warn) {
    max_length <- max(sapply(envir$counter_history, length))

    if (!is.null(start_after)) {            
      if (start_after > max_length) {                        
        warning(paste("Argument 'start_after=", start_after, 
                      "' exceeds maximum degree of sublayer nestedness (=", 
                      max_length, ").", sep=""))
      }
    }
    if (!is.null(stop_at)) {
      if (stop_at_0 > max_length){
        warning(paste("Argument 'stop_at=", stop_at_0, 
                      "' exceeds maximum degree of sublayer nestedness (=", 
                      max_length, ").", sep=""))    
      }
    }
  }
  
  out <- envir$out
  return(out)    
}

displayHTML <- function (html, browser="google-chrome", unlink=TRUE)
{
  f_tmp <- tempfile(fileext=".html")
  writeLines(html, f_tmp)
  browseURL(url=f_tmp, browser=browser)
  if (unlink) {
    Sys.sleep(2)
    unlink(f_tmp)
  }
}

