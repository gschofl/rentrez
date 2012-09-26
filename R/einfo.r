#' @include utils.r
#' @include eutil.r
NULL


# einfo-class ------------------------------------------------------------


#' einfo
#'
#' \dQuote{einfo} is a virtual S4 class that is extended by the 
#' \code{\linkS4class{einfoDbList}}, and \code{\linkS4class{einfoDb}} classes.
#' 
#' @rdname einfo
#' @export
#' @classHierarchy
#' @classMethods
setClass("einfo",
         representation("VIRTUAL"),
         contains = "eutil")


# show-method ------------------------------------------------------------


#' @autoImports
setMethod("show", "einfo",
          function (object) {
            if (is(object, "einfoDbList")) {
              cat("List of all valid Entrez databases\n")
              print(object@dbList)
              invisible()
            } else if (is(object, "einfoDb")) {
              cat(sprintf("Statistics for Entrez database %s\n", sQuote(object@menuName)))
              n <- slotNames(object)
              cat("@", n[1], "\n", sep="")
              print(object@dbName)
              cat("@", n[2], "\n", sep="")
              print(object@menuName)
              cat("@", n[3], "\n", sep="")
              print(object@description)
              cat("@", n[4], "\n", sep="")
              print(object@records)
              cat("@", n[5], "\n", sep="")
              print(object@lastUpdate)
              cat(paste0("@", n[6], paste0("$", names(object@fields))), "\n", sep=" ")
              print(object@fields$Name)
              cat(paste0("@", n[7], paste0("$", names(object@links))), "\n", sep=" ")
              print(object@links$Name)
              invisible()
            }
          })


# einfoDbList-class ------------------------------------------------------


#' einfoDbList
#' 
#' \dQuote{einfoDbList} is an S4 class that provides a container for data
#' retrived by calls to the 
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI EInfo}
#' utility.
#' 
#' @slot url A character vector containing the query URL.
#' @slot error Any error or warning messages parsed from
#' the output of the call submitted to Entrez.
#' @slot content An \code{\linkS4class{XMLInternalDocument}} object or
#' a character vector holding the unparsed output from the call
#' submitted to Entrez.
#' @slot dbList A list of the names of all valid Entrez databases.
#'
#' @rdname einfoDbList
#' @export
#' @classHierarchy
#' @classMethods
.einfoDbList <- setClass("einfoDbList",
                         representation(dbList = "character"),
                         prototype(dbList = NA_character_),
                         contains = "einfo")


# content-method ---------------------------------------------------------


setMethod("content", "einfoDbList",
          function (x, parse = TRUE) {
            if (isTRUE(parse)) {
              x@dbList
            } else {
              x@content
            }
          })


# subsetting-methods -----------------------------------------------------


setMethod("[", c("einfoDbList", "numeric", "missing", "ANY"),
          function (x, i, j, ..., drop = TRUE) {
            x@dbList[i]
          })


# einfoDb-class ----------------------------------------------------------


#' einfoDb
#' 
#' \dQuote{einfoDb} is an S4 class that provides a container for data
#' retrived by calls to the
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI EInfo}
#' utility.
#' 
#' @slot url A character vector containing the query URL.
#' @slot error Any error or warning messages parsed from
#' the output of the call submitted to Entrez.
#' @slot content An \code{\linkS4class{XMLInternalDocument}} object or
#' a character vector holding the unparsed output from the call
#' submitted to Entrez.
#' @slot dbName Name of the target database.
#' @slot menuName Name of the target database.
#' @slot descriptiom Short description of the target database.
#' @slot records Count of records in the target database.
#' @slot lastUpdate Last update of the target database.
#' @slot fields Field names of the target database.
#' @slot links Available links for the target database.
#' 
#' @rdname einfoDb
#' @export
#' @classHierarchy
#' @classMethods
.einfoDb <- setClass("einfoDb",
                     representation(dbName = "character",
                                    menuName = "character",
                                    description = "character",
                                    records = "numeric",
                                    lastUpdate = "POSIXlt",
                                    fields = "data.frame",
                                    links = "data.frame"),
                     prototype(dbName = NA_character_,
                               menuName = NA_character_,
                               description = NA_character_,
                               records = NA_integer_,
                               lastUpdate = as.POSIXlt(NA),
                               fields = data.frame(),
                               links = data.frame()),
                     contains = "einfo")


# content-method ---------------------------------------------------------


setMethod("content", "einfoDb",
          function (x, parse = TRUE) {
            if (isTRUE(parse)) {
              list(dbName = x@dbName,
                   menuName = x@menuName,
                   description = x@description,
                   records = x@records,
                   lastUpdate = x@lastUpdate,
                   fields = x@fields,
                   links = x@links
              )
            } else {
              x@content
            }
          })


#' \code{einfo} rtrieves information about each database in the NCBI Entrez
#' system. If no database is specified \code{einfo} will return the current
#' list of NCBI databases available for querying.
#' For specific databases, \code{einfo} returns the name, a description, the
#' number of records indexed in the database, the date of the last update of
#' the database, the fields and the available links from the database to
#' other Entrez databases.
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo}{EUtilities}
#' for additional information.
#' 
#' @param db \code{NULL} or a valid NCBI database name
#' @return An \code{einfo} object.
#' @export
#' @example inst/examples/einfo.r
#' @autoImports
einfo <- function (db=NULL) {
  if (is.null(db)) {
    o <- .query('einfo')
    .einfoDbList(url = o@url, content = o@content, error = checkErrors(o),
                 dbList = xpathSApply(o@content, '//DbList/DbName', xmlValue))
  } else {
    if (length(db) > 1L) {
      warning("Only the first database will be queried")
      db <- db[1L]
    }
    o <- .query('einfo', db=db)
    
    # extract FieldList elements
    fnm <- vapply(getNodeSet(o@content, '//FieldList/Field[1]/child::node( )'),
                  xmlName, character(1))
    if (not_empty(fnm)) {
      field_info <- as.data.frame(stringsAsFactors = FALSE,
                                  split(sapply(getNodeSet(o@content, '//FieldList/Field/*'),
                                               xmlValue), fnm))[, fnm]
    } else  {
      field_info <- data.frame()
    }
    
    # extract LinkList elements
    lnm <- sapply(getNodeSet(o@content, '//LinkList/Link[1]/child::node( )'), xmlName)
    if (not_empty(lnm)) {
      link_info <- as.data.frame(stringsAsFactors = FALSE,
                                 split(sapply(getNodeSet(o@content, '//LinkList/Link/*'),
                                              xmlValue), lnm))[, lnm]
    } else {
      link_info <- data.frame()
    }
    
    .einfoDb(url = o@url, content = o@content, error = checkErrors(o),
             dbName = xmlValue(xmlRoot(o@content)[[1L]][['DbName']]),
             menuName = xmlValue(xmlRoot(o@content)[[1L]][['MenuName']]),
             description = xmlValue(xmlRoot(o@content)[[1L]][['Description']]),
             records = as.numeric(xmlValue(xmlRoot(o@content)[[1L]][['Count']])),
             lastUpdate = as.POSIXlt(xmlValue(xmlRoot(o@content)[[1L]][['LastUpdate']])),
             fields = field_info,
             links = link_info)
  }
}
