## parse docsums (esummary) ####

#' @autoImports
docsum <- function (x, version) {
  
  if (identical(version, "default")) {
    nodes <- getNodeSet(x@content, '//DocSum')
    uids <- vapply(nodes, function (x) {
      xmlValue(xmlChildren(x)[["Id"]])
    }, character(1))
  } else if (identical(version, "2.0")) {
    nodes <- getNodeSet(x@content, '//DocumentSummary')
    uids <- vapply(nodes, xmlGetAttr, name="uid", FUN.VALUE=character(1))
  }
  
  docsum <- {
    docsum_list <- lapply(nodes, .parse_docsum)
    flat_docsum_list <- flatten(docsum_list, start_after=1, delim_path=".")
    
    # check if all docsums have same number of tags
    if (length(unique(vapply(flat_docsum_list, length, numeric(1)))) > 1L) {
      warning("DocSum records have unequal numbers of tags,\nso we cannot return a data frame.")
      flat_docsum_list
    } else
      data.frame(stringsAsFactors=FALSE, 
                 cbind(Id = uids, do.call(rbind, flat_docsum_list)))
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


## Parse Sequence data (efetch) ####


#' @autoImports
.parseSequence <- function (x, ...) {
  
  ## if rettype = fasta
  if (grepl("^fasta", x@type) && x@mode == "text") {
    return( .parseFasta(x = x, ...) )
  }
  
  ## if rettype = gp or gb
  if (grepl("^gb|^gp", x@type) && x@mode == "text") {
    return( .parseGb(x = x, ...) )
  }
  
  ## if retmode = xml return parsed xml tree
  if (x@mode == "xml") {
    return( xmlParse(x@content) )
  }
  
  ## otherwise return the raw content
  return( x@content )
}


#' @autoImports
.parseFasta <- function (x, format = 'Biostrings') {
  
  format <- match.arg(format, c("Biostrings", "DNAbin", "String"))
  
  if (!grepl("^>", x@content)) {
    warning("Does not appear to contain a valid fasta file")
    return( x@content )
  }
  
  if (x@database %in% c("nucleotide","nuccore")) {
    seqtype <- "DNA"
  } else if (x@database == "protein") {
    seqtype <- "AA"
  }
  
  if (format == "Biostrings") {
    f_tmp <- tempfile(fileext=".fa")
    write(x, file=f_tmp)
    fasta <- switch(seqtype,
                    DNA=tryCatch(read.DNAStringSet(f_tmp, use.names=TRUE),
                                 error = function (e) {
                                   read.AAStringSet(f_tmp, use.names=TRUE)
                                 }),              
                    AA=read.AAStringSet(f_tmp, use.names=TRUE))
    unlink(f_tmp)
    return( fasta )
  }
  
  if (format == "DNAbin") {
    fasta <- switch(seqtype,
                    DNA=ape::read.dna(file=textConnection(x@content), format="fasta"),
                    AA=phangorn::read.aa(file=textConnection(x@content), format="fasta"))
    return( fasta )  
  }
  
  if (format == "String") {
    fasta_split <- unlist(strsplit(x@content, "\n\n"))
    fasta <- lapply(fasta_split, function (fasta) {
      x <- unlist(strsplit(fasta, "\n"))
      desc_idx <- grep(pattern="^>", x)
      desc <- sub(">", "", x[desc_idx])
      x <- paste0(x[-desc_idx], collapse="")
      attr(x, "desc") <- desc
      x
    })
    return( fasta )
  }
}


#' @autoImports
.parseGb <- function (x, format = 'gbRecord') {
  
  format <- match.arg(format, 'gbRecord')
  
  if (format == "gbRecord") {
    
    dbs <- gbRecord(x, with_sequence=TRUE, force=FALSE)
    
    if (length(dbs) == 1L) {
      return(dbs)
    } else {
      records <- list()
      for (db in dbs) {
        records <- c(records, list(db))
      }
      names(records) <- vapply(records, "[[", "accession", FUN.VALUE=character(1))
      return(records)
    }
  }
}


## parse pubmed records (efetch) ####

#' @autoImports
.parsePubmed <- function (x) {
  
  if (x@mode != 'xml') {
    return( x@content )
  }
  
  doc <- getNodeSet(xmlRoot(xmlParse(x@content)), '//PubmedArticle')
  reff <- lapply(doc, function (art) {
    #     art <- xmlDoc(doc[[1]])
    art <- xmlDoc(art)
    
    author <- {
      lastName <- xpathApply(art, "//AuthorList//LastName", xmlValue)
      foreName <- xpathApply(art, "//AuthorList//ForeName", xmlValue)
      list(author = do.call(personList,
                            Map(person, given=foreName, family=lastName)))
    }
    
    issue <- list(
      volume = xpathSApply(art, '//JournalIssue/Volume', xmlValue),
      number = xpathSApply(art, '//JournalIssue/Issue', xmlValue),
      year = {
        year <- xpathSApply(art, '//JournalIssue/PubDate/Year', xmlValue)
        medlineDate <- xpathSApply(art, '//JournalIssue/PubDate/MedlineDate', xmlValue)
        if (length(year) > 0) year else medlineDate
      },
      month = xpathSApply(art, '//JournalIssue/PubDate/Month', xmlValue),
      pages = xpathSApply(art, '//Pagination/MedlinePgn', xmlValue)
    )
    
    journal <- list(
      issn = xpathSApply(art, '//Journal/ISSN', xmlValue),
      journal = xpathSApply(art, '//Journal/Title', xmlValue),
      abbrev = xpathSApply(art, '//Journal/ISOAbbreviation', xmlValue) 
    )
    
    article <- list(
      title = xpathSApply(art, '//ArticleTitle', xmlValue),
      abstract = {
        abs <- xpathSApply(art, '//Abstract/AbstractText', xmlValue)
        headers <- xpathSApply(art, '//Abstract/AbstractText', xmlGetAttr, "Label")
        if (is.null(headers[[1]])) {
          abs
        } else {
          paste0(headers, ": ", abs, collapse="\n")
        }
      },
      doi = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="doi"]', xmlValue),
      pii = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pii"]', xmlValue),
      pmid = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pubmed"]', xmlValue),
      pmc = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pmc"]', xmlValue)
    )
    
    affiliation <- list(
      affiliation = xpathSApply(art, "//Affiliation", xmlValue)
    )
    
    issue[vapply(issue, function (x) length(x) < 1L, logical(1))] <- ""
    journal[vapply(journal, function (x) length(x) < 1L, logical(1))] <- ""
    article[vapply(article, function (x) length(x) < 1L, logical(1))] <- ""
    affiliation[vapply(affiliation, function (x) length(x) < 1L, logical(1))] <- ""
    
    free(art)
    
    ref <- bibentry('Article', other=c(author, article, journal, issue, affiliation))
    ref
  
  })
  
  reff <- do.call("c", reff)
  reff
  
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

