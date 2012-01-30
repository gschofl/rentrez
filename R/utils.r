# Construct url, fetch response, construct eutil object
.query <- function (eutil, ...) {
  stopifnot(require(XML))
  stopifnot(require(RCurl))
  eutils_host <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/'
  query_string <- .query_string(...)
  url <- sprintf('%s%s.fcgi%s', eutils_host, eutil, query_string)
  new("eutil",
      url = curlUnescape(url),
      xml = xmlTreeParse(getURL(url), useInternalNodes = TRUE))
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

checkErrors <- function (obj) {
  error <- NULL
  err_msgs <- NULL
  wrn_msgs <- NULL
  
  err_node <- getNodeSet(slot(obj, "xml"), '//ERROR')
  if (length(err_node) > 0)
    error <- lapply(err_node, xmlValue)
  
  err_list_node <- getNodeSet(slot(obj, "xml"), '//ErrorList')
  if (length(err_list_node) > 0)
    err_msgs <- lapply(xmlChildren(err_list_node[[1]]), xmlValue)
  
  wrn_list_node <- getNodeSet(slot(obj, "xml"), '//WarningList')
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