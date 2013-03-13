## parse docsums (esummary) ####
#' @autoImports
.docsum <- function (x) {
  
  nodes <- getNodeSet(x, '//DocSum')
  if (!all_empty(nodes)) {
    uids <- xvalue(x, '/eSummaryResult/DocSum/Id')
  } else {
    nodes <- getNodeSet(x, '//DocumentSummary')
    if (all_empty(nodes)) {
      if (!all_empty(checkErrors(x, verbose=FALSE))) {
        warning("Errors parsing DocumentSummary", call.=FALSE)
      }
      return( list() )
    } else {
      uids <- vapply(nodes, xmlGetAttr, name="uid", FUN.VALUE=character(1))
    }
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
#' @importFrom XML xmlSize
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
    id <- xvalue(idUrl, "/IdUrlSet/Id") 
    objUrlSet <- getNodeSet(idUrl, "//ObjUrl")
    urlset <- lapply(objUrlSet, function (objUrl) {
      objUrl <- xmlDoc(objUrl)
      l <- list(
        url = xvalue(objUrl, '/ObjUrl/Url'),
        iconUrl = xvalue(objUrl, '/ObjUrl/IconUrl'),
        linkName = xvalue(objUrl, '/ObjUrl/LinkName'),
        subjectType = xvalue(objUrl, "/ObjUrl/SubjectType"),
        category = xvalue(objUrl, "/ObjUrl/Category"),
        attribute = paste0(xvalue(objUrl, "/ObjUrl/Attribute"), collapse=";"),
        provider.name = xvalue(objUrl, "//Provider/Name"),
        provider.nameAbbr = xvalue(objUrl, "//Provider/NameAbbr"),
        provider.id = xvalue(objUrl, "//Provider/Id"),
        provider.url = xvalue(objUrl, "//Provider/Url"),
        provider.iconUrl = xvalue(objUrl, "//Provider/IconUrl")
      )
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
  dbFrom <- xvalue(content, "//DbFrom")
  id <- xvalue(content, "//Id")
  has_neighbor <- xattr(content, "//Id", "HasNeighbor")
  has_linkout <- xattr(content, "//Id", "HasLinkOut")

  chklst <- if (not.na(has_neighbor)) {
    data.frame(stringsAsFactors=FALSE, Id=id,
               HasNeighbor=ifelse(has_neighbor == "Y", TRUE, FALSE))
  } else if  (not.na(has_linkout)) {
    data.frame(stringsAsFactors=FALSE, Id=id,
               HasLinkOut=ifelse(has_linkout == "Y", TRUE, FALSE))
  }
  
  chklst
}

# Parse a LinkSet and return it as a data.frame
#' @autoImports
.parseIdLinkSet <- function (content) {
  content <- xmlRoot(content)
  dbFrom <- xvalue(content, "//DbFrom")
  idLinkSet <- getNodeSet(xmlRoot(content), "//IdLinkSet")
  
  if (length(idLinkSet) < 1L)
    return(list())
  
  ll <- lapply(idLinkSet, function (ls) {
    ls <- xmlDoc(ls)
    Id <- xvalue(ls, "(//Id)[1]")
    link_info <- 
      lapply(getNodeSet(ls, "//LinkInfo"), function (li) {
        li <- xmlDoc(li)
        li <- list(DbTo = xvalue(li, "//DbTo"), 
                   LinkName = xvalue(li, "//LinkName"),
                   MenuTag = xvalue(li, "//MenuTag"),
                   HtmlTag = xvalue(li, "//HtmlTag"),
                   Priority = xvalue(li, "//Priority"))
        li
      })
    data.frame(stringsAsFactors=FALSE, Id=Id, 
               do.call(rbind, link_info))
  })
  
  ll
}

# Parse a LinkSet and return it as a named list
#' @autoImports
.parseLinkSet <- function (response) {
  linkSetDb <- getNodeSet(xmlRoot(response), "//LinkSetDb")
  
  if (length(linkSetDb) < 1L)
    return(list())
  
  ll <- lapply(linkSetDb, function(lsd) {
    lsd <- xmlDoc(lsd)
    id <- xvalue(lsd, "//Id")
    score <- xvalue(lsd, "//Score")
    ans <- compact(list(id=id, score=score))
    ans
  })
  
  names(ll) <- xvalue(xmlRoot(response), "//LinkName")
  ll
}
