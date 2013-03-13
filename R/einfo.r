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
#' @slot content A character vector holding the unparsed
#' contents of a request to Entrez.
#' @slot dbName A list of the names of all valid Entrez databases.
#'
#' @rdname einfoDbList
#' @export
#' @classHierarchy
#' @classMethods
setClass("einfoDbList",
         representation(dbName = "character"),
         prototype(dbName = NA_character_),
         contains = "einfo")


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
#' @slot content A character vector holding the unparsed
#' contents of a request to Entrez.
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
setClass("einfoDb",
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


# einfo accessors --------------------------------------------------------


setMethod("dbName", "einfo", function(x) x@dbName)

setMethod("menuName", "einfoDb", function(x) x@menuName)

setMethod("description", "einfoDb", function(x) x@description)

setMethod("records", "einfoDb", function(x) x@records)

setMethod("lastUpdate", "einfoDb", function(x) x@lastUpdate)

setMethod("fields", "einfoDb", function(x) x@fields)

setMethod("links", "einfoDb", function(x) x@links)


# subsetting-methods -----------------------------------------------------


setMethod("[", c("einfoDbList", "numeric", "missing", "ANY"),
          function (x, i, j, ..., drop = TRUE) {
            initialize(x, dbName = dbName(x)[i])
          })


# show-method ------------------------------------------------------------


#' @autoImports
setMethod("show", "einfo",
          function (object) {
            if (is(object, "einfoDbList")) {
              cat("List of Entrez databases\n")
              print(dbName(object))
              invisible(NULL)
            } else if (is(object, "einfoDb")) {
              cat(sprintf("Statistics for Entrez database %s\n",
                          sQuote(menuName(object))))
              n <- slotNames(object)
              cat(n[1], ":\n", sep="")
              print(dbName(object))
              cat(n[2], ":\n", sep="")
              print(menuName(object))
              cat(n[3], ":\n", sep="")
              print(description(object))
              cat(n[4], ":\n", sep="")
              print(records(object))
              cat(n[5], ":\n", sep="")
              print(lastUpdate(object))
              cat(paste0(n[6], paste0("$", names(fields(object)))),
                  "\n", sep=" ")
              print(fields(object)$Name)
              cat(paste0(n[7], paste0("$", names(links(object)))),
                  "\n", sep=" ")
              print(links(object)$Name)
              invisible(NULL)
            }
          })


#' \code{einfo} retrieves information about each database in the NCBI Entrez
#' system. If no database is specified \code{einfo} will return a list of
#' currently available NCBI databases.
#' For specific databases, \code{einfo} returns the name, a description, the
#' number of records indexed in the database, the date of the last update of
#' the database, the fields and the available links from the database to
#' other Entrez databases.
#' 
#' @details
#' See the documentation for the NCBI
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo}{EUtilities}
#' for additional information.
#' 
#' @param db A valid NCBI database name. If missing a list of all current NCBI
#' databases is returned.
#' @return An \code{einfo} instance.
#' @export
#' @example inst/examples/einfo.r
#' @autoImports
einfo <- function (db) {
  if (missing(db)) {
    o <- .equery('einfo')
    error <- if (all_empty(error(o))) checkErrors(o, FALSE) else error(o)
    
    if (all_empty(error)) {
      new("einfoDbList", url = queryUrl(o), content = content(o), error = error,
          dbName =  xvalue(content(o, "xml"), '//DbList/DbName'))
    } else {
      new("einfoDbList", url = queryUrl(o), content = content(o), error = error)
    }
   
  } else {
    if (length(db) > 1L) {
      warning("Only the first database will be queried")
      db <- db[1L]
    }
    o <- .equery('einfo', 'GET', db=db)
    error <- if (all_empty(error(o))) checkErrors(o, FALSE) else error(o)
    
    if (all_empty(error)) {
      response <- content(o, "xml")
      # extract FieldList elements
      fnm <- unique(xname(response, '//FieldList/Field/child::node()'))        
      if (!all_empty(fnm)) {
        fieldNodes <- getNodeSet(response, '//FieldList/Field/*')
        fieldList <- split(vapply(fieldNodes, xmlValue, character(1)), fnm)
        field_info <- data.frame(stringsAsFactors = FALSE, fieldList)[, fnm]
      } else  {
        field_info <- data.frame()
      }
      # extract LinkList elements
      lnm <- unique(xname(response, '//LinkList/Link/child::node( )'))
      if (!all_empty(lnm)) {
        linkNodes <- getNodeSet(response, '//LinkList/Link/*')
        linkList <- split(vapply(linkNodes, xmlValue, character(1)), lnm)
        link_info <- data.frame(stringsAsFactors = FALSE, linkList)[, lnm]
      } else {
        link_info <- data.frame()
      }
      new("einfoDb", url = queryUrl(o), content = content(o), error = error,
          dbName = xvalue(response, '//DbInfo/DbName'),
          menuName = xvalue(response, '//DbInfo/MenuName'),
          description = xvalue(response, '//DbInfo/Description'),
          records = xvalue(response, '//DbInfo/Count', as='integer'),
          lastUpdate = xvalue(response, '//DbInfo/LastUpdate', as="POSIXlt"),
          fields = field_info,
          links = link_info)
    } else {
      new("einfoDb", url = queryUrl(o), content = content(o), error = error)
    }
  }
}
