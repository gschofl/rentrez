
# einfo-class ------------------------------------------------------------

##' @include utils.r
##' @include eutil.r
##' @include blast-classes.r
NULL

##' einfo class
##'
##' einfo is a virtual S4 class that is extended by the 
##' \code{\link{einfoDbList-class}}, and
##' \code{\link{einfoDb-class}} classes.
##' 
##' @seealso \code{\link{einfo}} for generating calls to the NCBI EInfo
##' utility.
##' 
##' @name einfo-class
##' @rdname einfo-class
##' @exportClass einfo
##' @aliases show,einfo-method
##' @aliases einfo,einfo-method 
setClass("einfo",
         representation("VIRTUAL"),
         contains = "eutil")


# show-method ------------------------------------------------------------


##' @export
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
              cat(paste("@", n[6], paste("$", names(object@fields), sep=""), sep=""), "\n", sep=" ")
              print(object@fields$Name)
              cat(paste("@", n[7], paste("$", names(object@links), sep=""), sep=""), "\n", sep=" ")
              print(object@links$Name)
              invisible()
            }
          })


##' einfoDbList class
##' 
##' \sQuote{einfoDbList} is an S4 class that extends the
##' \code{\link{einfo-class}}.
##' This class provides a container for data retrived by calls to the
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI EInfo}
##' utility.
##' 
##' einfoDbList objects have one slots in addition to the slots provided by
##' basic \code{\link{eutil-class}} objects:
##' \describe{
##'   \item{dbList}{A list of the names of all valid Entrez databases}
##' }
##' 
##' @seealso \code{\link{einfo}} for generating calls to the NCBI EInfo
##' utility.
##' 
##' @name einfoDbList-class
##' @rdname einfoDbList-class
##' @exportClass einfoDbList
##' @aliases content,einfoDbList-method
.einfoDbList <-
  setClass("einfoDbList",
           representation(dbList = "character"),
           prototype(dbList = NA_character_),
           contains = "einfo")

##' einfoDb class
##' 
##' \sQuote{einfoDb} is an S4 class that extends the 
##' \code{\link{einfo-class}}.
##' This class provides a container for data retrived by calls to the
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI EInfo}
##' utility.
##' 
##' einfoDb objects have seven slots in addition to the slots provided by
##' basic \code{\link{eutil-class}} objects:
##' \describe{
##'   \item{dbName}{Name of the target database}
##'   \item{menuName}{Name of the target database}
##'   \item{descriptiom}{Short description of the target database}
##'   \item{records}{Count of records in the target database}
##'   \item{lastUpdate}{Last update of the target database}
##'   \item{fields}{Field names of the target database}
##'   \item{links}{Available links for the target database}
##' }
##' 
##' @seealso \code{\link{einfo}} for generating calls to the NCBI EInfo
##' utility.
##' 
##' @name einfoDb-class
##' @rdname einfoDb-class
##' @exportClass einfoDb
.einfoDb <- 
  setClass("einfoDb",
           representation(dbName = "character",
                          menuName = "character",
                          description = "character",
                          records = "numeric",
                          lastUpdate = "POSIXlt",
                          fields = "data.frame",
                          links = "data.frame"),
           prototype(dbName = NA_character_, menuName = NA_character_,
                     description = NA_character_, records = NA_integer_,
                     lastUpdate = as.POSIXlt(NA), fields = data.frame(),
                     links = data.frame()),
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

# subsetting-methods -----------------------------------------------------


##' @export
setMethod("[", c("einfoDbList", "numeric", "missing", "ANY"),
          function (x, i, j, ..., drop = TRUE) {
            x@dbList[i]
          })


# einfo ------------------------------------------------------------------


##' Retrieve information about each database in the NCBI Entrez system
##'
##' If no database is provided \code{einfo} will return the current list of
##' NCBI databases available for querying.
##' For specific databases, \code{einfo} provides
##' the name, a description, the number of records indexed in the database,
##' the date of the last update of the database, the fields and the
##' available links from the database to other Entrez databases.
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo}{EUtilities}
##' for additional information.
##' 
##' @param db \code{NULL} or a valid NCBI database name
##'
##' @return An \code{\link{einfo-class}} object.
##'
##' @export
##' @example inst/examples/einfo.r
einfo <- function (db=NULL) {
  if (is.null(db)) {
    o <- .query('einfo')
    .einfoDbList(url = o@url, content = o@content,
                 dbList = xpathSApply(o@content, '//DbList/DbName', xmlValue))
  } else {
    if (length(db) > 1L) {
      warning("Only the first database will be queried")
      db <- db[1L]
    }
    o <- .query('einfo', db=db)
    
    # extract FieldList elements
    fnm <- sapply(getNodeSet(o@content, '//FieldList/Field[1]/child::node( )'), xmlName)
    if (length(fnm) > 0L) {
      field_info <- as.data.frame(stringsAsFactors = FALSE,
                                  split(sapply(getNodeSet(o@content, '//FieldList/Field/*'),
                                               xmlValue), fnm))[, fnm]
    } else  {
      field_info <- data.frame()
    }
    # extract LinkList elements
    lnm <- sapply(getNodeSet(o@content, '//LinkList/Link[1]/child::node( )'), xmlName)
    if (length(lnm) > 0L) {
      link_info <- as.data.frame(stringsAsFactors = FALSE,
                                 split(sapply(getNodeSet(o@content, '//LinkList/Link/*'),
                                              xmlValue), lnm))[, lnm]
    } else {
      link_info <- data.frame()
    }
    
    .einfoDb(url = o@url, content = o@content,
             error = checkErrors(o),
             dbName = xmlValue(xmlRoot(o@content)[[1L]][['DbName']]),
             menuName = xmlValue(xmlRoot(o@content)[[1L]][['MenuName']]),
             description = xmlValue(xmlRoot(o@content)[[1L]][['Description']]),
             records = as.numeric(xmlValue(xmlRoot(o@content)[[1L]][['Count']])),
             lastUpdate = as.POSIXlt(xmlValue(xmlRoot(o@content)[[1L]][['LastUpdate']])),
             fields = field_info,
             links = link_info)
  }
}
