## parse docsums (esummary) ####
#' @autoImports
.docsum <- function (x, version) {
  
  if (identical(version, "default")) {
    nodes <- getNodeSet(x, '//DocSum')
    uids <- vapply(nodes, function (x) {
      xmlValue(xmlChildren(x)[["Id"]])
    }, character(1))
  } else if (identical(version, "2.0")) {
    nodes <- getNodeSet(x, '//DocumentSummary')
    uids <- vapply(nodes, xmlGetAttr, name="uid", FUN.VALUE=character(1))
  }
  
  docsum <- {
    docsum_list <- lapply(nodes, .parse_docsum)
    flat_docsum_list <- flatten(docsum_list, start_after=1, delim_path=".")
    # check if all docsums have same number of tags
    if (length(unique(vapply(flat_docsum_list, length, numeric(1)))) > 1L) {
      warning("DocSum records have unequal numbers of tags,\nso we cannot return a data frame.")
      flat_docsum_list
    } else {
      flat_docsum_list <- lapply(flat_docsum_list, unlist)
      data.frame(stringsAsFactors=FALSE, 
                 cbind(Id = uids, do.call(rbind, flat_docsum_list)))
    }
  }
  
  docsum
}

# Parse a DocSum recursively and return it as a named list
#' @autoImports
.parse_docsum <- function (ds) {
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
  } else if (xmlName(ds) == "DocumentSummary") {
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


## Parse linksets (elink) ####

#' @autoImports
.parseIdUrlList <- function (content) {
  content <- xmlRoot(content)
  idUrlSet <- getNodeSet(content, "//IdUrlSet")
  idurls <- lapply(idUrlSet, function (idUrl) {
    idUrl <- xmlDoc(idUrl)
    id <- xpathSApply(idUrl, "/IdUrlSet/Id", xmlValue) 
    objUrlSet <- getNodeSet(idUrl, "//ObjUrl")
    urlset <- lapply(objUrlSet, function (objUrl) {
      objUrl <- xmlDoc(objUrl)
      l <- list(
        url = unlist(xpathApply(objUrl, "/ObjUrl/Url", xmlValue)),
        iconUrl = unlist(xpathApply(objUrl, "/ObjUrl/IconUrl", xmlValue)),
        linkName = unlist(xpathApply(objUrl, "/ObjUrl/LinkName", xmlValue)),
        subjectType = unlist(xpathApply(objUrl, "/ObjUrl/SubjectType", xmlValue)),
        category = unlist(xpathApply(objUrl, "/ObjUrl/Category", xmlValue)),
        attribute = paste0(unlist(xpathApply(objUrl, "/ObjUrl/Attribute", xmlValue)), collapse=";"),
        provider.name = unlist(xpathApply(objUrl, "//Provider/Name", xmlValue)),
        provider.nameAbbr = unlist(xpathApply(objUrl, "//Provider/NameAbbr", xmlValue)),
        provider.id = unlist(xpathApply(objUrl, "//Provider/Id", xmlValue)),
        provider.url = unlist(xpathApply(objUrl, "//Provider/Url", xmlValue)),
        provider.iconUrl = unlist(xpathApply(objUrl, "//Provider/IconUrl", xmlValue))
      )
      l[vapply(l, is.null, logical(1))] <- ""
      data.frame(stringsAsFactors=FALSE, l)
    })
    structure(do.call(rbind, urlset), id = id)
  })
  names(idurls) <- vapply(idurls, attr, "id", FUN.VALUE=character(1))
  idurls
}

# Parse IdCheckList returned from cmd=ncheck,lcheck
#' @autoImports
.parseIdCheckList <- function (content) {
  content <- xmlRoot(content)
  dbFrom <- xpathSApply(content, "//DbFrom", xmlValue)
  id <- xpathSApply(content, "//Id", xmlValue)
  has_neighbor <- unlist(xpathApply(content, "//Id", xmlGetAttr, "HasNeighbor"))
  has_linkout <- unlist(xpathApply(content, "//Id", xmlGetAttr, "HasLinkOut"))
  
  chklst <- if (not.null(has_neighbor)) {
    data.frame(stringsAsFactors=FALSE, Id=id,
               HasNeighbor=ifelse(has_neighbor == "Y", TRUE, FALSE))
  } else if  (!is.null(has_linkout)) {
    data.frame(stringsAsFactors=FALSE, Id=id,
               HasLinkOut=ifelse(has_linkout == "Y", TRUE, FALSE))
  }
  
  chklst
}

# Parse a LinkSet and return it as a data.frame
#' @autoImports
.parseIdLinkSet <- function (content) {
  content <- xmlRoot(content)
  dbFrom <- xpathSApply(content, "//DbFrom", xmlValue)
  idLinkSet <- getNodeSet(xmlRoot(content), "//IdLinkSet")
  
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
#' @autoImports
.parseLinkSet <- function (content) {
  linkSetDb <- getNodeSet(xmlRoot(content), "//LinkSetDb")
  
  if (length(linkSetDb) < 1L)
    return(list())
  
  ll <- lapply(linkSetDb, function(lsd) {
    lsd <- xmlDoc(lsd)
    id <- xpathSApply(lsd, "//Id", xmlValue)
    score <- xpathSApply(lsd, "//Score", xmlValue)
    ans <- list(id=id, score=score)
    ans[vapply(ans, length, integer(1)) == 0L] <- NULL
    ans
  })
  
  names(ll) <- xpathSApply(xmlRoot(content), "//LinkName", xmlValue)
  ll
}
