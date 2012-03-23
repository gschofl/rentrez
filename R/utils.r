#' Construct url, fetch response, construct eutil object
#' 
#' @importFrom RCurl curlUnescape
#' @importFrom RCurl getURL
#' @keywords internal
.query <- function (eutil, ...) {
  eutils_host <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/'
  query_string <- .query_string(...)
  #query_string <- .query_string(..., tool="Rentrez", email="gschofl@yahoo.de")
  
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

.query_string <- function (...) {
  args <- list(...)
  params <- names(args)
  empty <- sapply(args, is.null)
  fields <- paste(as.character(params[!empty]), as.character(args[!empty]), sep="=")
  .escape(paste("?", paste(fields, collapse="&"), sep=""))
}

.escape <- function (s, httpPOST=FALSE) {
  if (httpPOST) {
    s <- gsub(" +", " ", s)
    s <- gsub("+", " ", s, fixed=TRUE)
  }
  else {
    s <- gsub(" +", "\\+", s)
  }
  s <- paste(strsplit(s, '\"', fixed=TRUE)[[1L]], collapse="%22")
  s <- gsub("\\#", "%23", s)
  s <- gsub("\\+(and)\\+|\\+(or)\\+|\\+(not)\\+","\\+\\U\\1\\U\\2\\U\\3\\+", s, perl=TRUE)
  s
}

#' use HTTP POST
#' 
#' @importFrom RCurl postForm
#' @importFrom RCurl curlOptions
#' @keywords internal
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
.parseDocSumItems <- function (items) {
  items <- 
    xmlChildren(items, addNames=FALSE)[names(xmlChildren(items)) == "Item"]
  value <- 
    lapply(items, function (item) {
      if (!xmlSize(item) > 1)
        xmlValue(item)
      else
        .parseDocSumItems(item)
    })
  names(value) <- 
    lapply(items, function (item) xmlGetAttr(item, "Name"))
  return(value)
}

# Parse a LinkSet and return it as a named list
.parseLinkSet <- function (data) {
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

isEmpty <- function (x) length(x) == 0L

checkErrors <- function (obj) {
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

.collapse <- function (id) paste0(id, collapse = ",")

.getDb <- function (object) {
  if (is(object, "esearch") || is(object, "epost"))
    db <- object@database
  else if (is(object, "elink"))
    db <- object@databaseTo
  else
    db <- NULL
  db
}

.getId <- function (object) {
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
  else if (is(object, "esearch")) {
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

.docsum.sequence <- function (esummary) {
  ds <- esummary@documentSummary
  nr <- length(ds)
  fr <- data.frame(stringsAsFactors=FALSE, uid=character(nr),
                   caption=character(nr), title=character(nr),
                   extra=character(nr), gi=character(nr),
                   created=character(nr), updated=character(nr),
                   flags=character(nr), tax_id=character(nr),
                   length=numeric(nr), status=character(nr),
                   replaced_by=character(nr), comment=character(nr))
  
  for (i in seq.int(nr))
    fr[i,] <- c(names(ds[i]), ds[[i]])

  fr$created <- as.Date(fr$created, "%Y/%m/%d")
  fr$updated <- as.Date(fr$updated, "%Y/%m/%d")
  fr$length <- as.numeric(fr$length)
  row.names(fr) <- NULL
  fr
}

.docsum.genome <- function (esummary) {
  ds <- esummary@documentSummary
  nr <- length(ds)
  fr <- data.frame(stringsAsFactors=FALSE, uid=character(nr),
                   name=character(nr), kingdom=character(nr),
                   defline=character(nr), pid=character(nr),
                   chromosomes=character(nr), plasmids=character(nr),
                   organelles=character(nr), assembly=character(nr),
                   accession=character(nr), assembly_id=character(nr),
                   created=character(nr), options=character(nr))
  
  for (i in seq.int(nr))
    fr[i,] <- c(names(ds[i]), ds[[i]])
  
  fr$created <- as.Date(strsplit(fr$created, " ")[[1L]][1L], "%Y/%m/%d")
  row.names(fr) <- NULL
  fr
}

.docsum.pubmed <- function (esummary) {
  ds <- esummary@documentSummary
  nr <- length(ds)
  fr <- data.frame(stringsAsFactors=FALSE, pmid=character(nr),
                   authors=character(nr), year=character(nr),
                   title=character(nr), journal=character(nr),
                   volume=character(nr), issue=character(nr),
                   pages=character(nr), doi=character(nr),
                   epubdate=character(nr))

  for (i in seq.int(nr))
    fr[i,] <- c(names(ds[i]), 
                paste(ds[[i]]$AuthorList, collapse=", "),
                substr(ds[[i]]$PubDate, 1, 4) , ds[[i]]$Title,
                ds[[i]]$Source, ds[[i]]$Volume,
                ds[[i]]$Issue, ds[[i]]$Pages,
                if (!is.null(ds[[i]]$DOI)) ds[[i]]$DOI else "",
                ds[[i]]$EPubDate)
      
  fr$epubdate <- as.Date(fr$epubdate, "%Y %b %d")
  row.names(fr) <- NULL
  fr
}

.docsum.taxonomy <- function (esummary) {
  ds <- esummary@documentSummary
  nr <- length(ds)
  fr <- data.frame(stringsAsFactors=FALSE, rank=character(nr),
                   division=character(nr), scientific_name=character(nr),
                   common_name=character(nr), txid=character(nr),
                   nucleotide=character(nr), protein=character(nr),
                   structure=character(nr), genome=character(nr),
                   gene=character(nr), genus=character(nr),
                   species=character(nr), subspecies=character(nr))
  
  for (i in seq.int(nr)) fr[i,] <- ds[[i]]
  
  fr <- data.frame(fr[5], fr[c(1,2,3,4,6,7,8,9,10,11,12,13)])
  fr$nucleotide <- as.numeric(fr$nucleotide)
  fr$protein <- as.numeric(fr$protein)
  fr$structure <- as.numeric(fr$structure)
  fr$genome <- as.numeric(fr$genome)
  fr$gene <- as.numeric(fr$gene)
  row.names(fr) <- NULL
  fr
}

