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

