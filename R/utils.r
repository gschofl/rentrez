# Construct url, fetch response, construct eutil object
.query <- function (eutil, ...) {
  eutils_host <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/'
  query_string <- .query_string(...)
  
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

.escape <- function (s) {
  s <- paste(strsplit(s, '\"', fixed=TRUE)[[1]], collapse="%22")
  # s <- gsub("\\[", "%5B", s)
  # s <- gsub("\\]", "%5D", s)
  s <- gsub("\\#", "%23", s)
  s <- gsub("\\ (and)\\ |\\ (or)\\ |\\ (not)\\ ","\\ \\U\\1\\U\\2\\U\\3\\ ", s, perl=TRUE)
  gsub(" +", "\\+", s)
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
    return(NULL)
  
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

collapseUIDs <- function (id) {
  
  if (is(id, "esearch"))
    id <- id@idList
  
  if (length(id) > 1L) {
    if (length(id) > 200L) {
      warning("The UID list is too large. Only the first 200 UIDs will be used")
      id <- id[1:200]
    }
    id <- paste(id, collapse = ",")
  }
  id
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
  fr <- fr[order(fr$title),]
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
  fr <- fr[order(fr$name),]
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
  fr <- fr[order(fr$authors),]
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
  fr <- fr[order(fr$scientific_name),]
  row.names(fr) <- NULL
  fr
}

