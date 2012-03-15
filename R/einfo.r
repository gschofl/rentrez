### Einfo ##################################################################
##' @include eutil.r
##' @include utils.r
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
##' @aliases einfo,einfo-method
##' @aliases show,einfo-method
setClass("einfo", representation("VIRTUAL"), contains = "eutil")


##' einfoDbList class
##' 
##' einfoDbList is an S4 class that extends the \code{\link{einfo-class}}.
##' This class provides a container for data retrived by calls to the 
##' NCBI EInfo utility.
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
##' @aliases [,einfoDbList-method
setClass("einfoDbList",
         representation(dbList = "charOrNULL"),
         prototype(dbList = NULL),
         contains = "einfo")


##' einfoDb class
##' 
##' einfoDb is an S4 class that extends the \code{\link{einfo-class}}.
##' This class provides a container for data retrived by calls to the 
##' NCBI EInfo utility.
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
setClass("einfoDb",
         representation(dbName = "charOrNULL",
                        menuName = "charOrNULL",
                        description = "charOrNULL",
                        records = "numOrNULL",
                        lastUpdate = "POSOrNULL",
                        fields = "dfOrNULL",
                        links = "dfOrNULL"),
         prototype(dbName = NULL, menuName = NULL,
                   description = NULL, records = NULL,
                   lastUpdate = NULL, fields = NULL,
                   links = NULL),
         contains = "einfo")

### show method ############################################################

##' @export
setMethod("show",
          signature(object = "einfo"),
          function (object) {
            if (is(object, "einfoDbList")) {
              cat("List of all valid Entrez databases\n")
              print(object@dbList)
              return(invisible(NULL))
            }
            else if (is(object, "einfoDb")) {
              cat(paste("Statistics for Entrez", slot(object, "menuName"), "\n"))
              n <- slotNames(object)
              cat("$", n[1], "\n", sep="")
              print(slot(object, "dbName"))
              cat("$", n[2], "\n", sep="")
              print(slot(object, "menuName"))
              cat("$", n[3], "\n", sep="")
              print(slot(object, "description"))
              cat("$", n[4], "\n", sep="")
              print(slot(object, "records"))
              cat("$", n[5], "\n", sep="")
              print(slot(object, "lastUpdate"))
              cat(paste("$", n[6], paste("$", names(object$fields), sep=""), sep=""), "\n", sep=" ")
              print(slot(object, "fields")$Name)
              cat(paste("$", n[7], paste("$", names(object$links), sep=""), sep=""), "\n", sep=" ")
              print(slot(object, "links")$Name)
              return(invisible(NULL))
            }
          })

### extract methods ########################################################

##' @export
setMethod("[",
          signature(x="einfoDbList", i="numeric"),
          function (x, i, j, ..., drop) {
            x@dbList[i]
          })

##' Retrieve information about each database in the NCBI Entrez system
##'
##' If no database is provided \code{einfo} will return the current list of
##' NCBI databases available for querying.
##' For specific databases, \code{einfo} provides
##' the name, a description, the number of records indexed in the database,
##' the date of the last update of the database, the fields and the
##' available links from the database to other Entrez databases.
##' 
##' See the online documentation at
##' \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo}
##' for additional information.
##' 
##' @param db \code{NULL} or a valid NCBI database name
##'
##' @return An \code{\link{einfo-class}} object.
##'
##' @export
##'
##' @examples
##'   databases <- einfo()
einfo <- function (db=NULL) {
  if (is.null(db)) {
    o <- .query(eutil='einfo')
    new("einfoDbList", url=o@url, data=o@data,
        dbList=as.character(xpathSApply(o@data, '//DbList/DbName', xmlValue)))
  }
  else {
    if (length(db) > 1L) {
      warning("Only the first database will be queried")
      db <- db[1L]
    }
    o <- .query(eutil='einfo', db=db)

    # extract FieldList elements
    fnm <- sapply(getNodeSet(o@data, '//FieldList/Field[1]/child::node( )'), xmlName)
    if (length(fnm) > 0L)
      field_info <- as.data.frame(stringsAsFactors = FALSE,
                                  split(sapply(getNodeSet(o@data, '//FieldList/Field/*'),
                                               xmlValue), fnm))[, fnm]
    else
      field_info <- data.frame()

    # extract LinkList elements
    lnm <- sapply(getNodeSet(o@data, '//LinkList/Link[1]/child::node( )'), xmlName)
    if (length(lnm) > 0L)
      link_info <- as.data.frame(stringsAsFactors = FALSE,
                                 split(sapply(getNodeSet(o@data, '//LinkList/Link/*'),
                                              xmlValue), lnm))[, lnm]
    else
      link_info <- data.frame()

    new("einfoDb", url=o@url, data=o@data,
        error=checkErrors(o),
        dbName=xmlValue(xmlRoot(o@data)[[1L]][['DbName']]),
        menuName=xmlValue(xmlRoot(o@data)[[1L]][['MenuName']]),
        description=xmlValue(xmlRoot(o@data)[[1L]][['Description']]),
        records=as.numeric(xmlValue(xmlRoot(o@data)[[1L]][['Count']])),
        lastUpdate=as.POSIXlt(xmlValue(xmlRoot(o@data)[[1L]][['LastUpdate']])),
        fields=field_info,
        links=link_info)
  }
}

### Convenience functions ##################################################

##' Retrieve the NCBI databases available for querying.
##'
##' @return Returns a character vector of database names.
##'
##' @export
##' @examples
##'    listDatabases()
listDatabases <- function () {
  return(slot(einfo(), "dbList"))
}

##' Retrieve field codes for an NCBI database
##'
##' @param db An NCBI database
##'
##' @param all if \code{FALSE} only field names, full names, and
##'   field descriptions are returned.
##'
##' @return A data frame
##'
##' @export
##' @examples
##'    pmf <- listFields("pubmed")
##'    pmf$Name
listFields <- function (db, all=FALSE) {
  if (missing(db))
    stop("No database specified")
  if (!is.character(db))
    stop("db argument in no string. This argument should be a single character string")

  fields <- slot(einfo(db), "fields")
  if (isTRUE(all))
    return(fields)
  else if (!isTRUE(all))
    return(fields[,c("Name", "FullName", "Description")])
}


##' Retrieve links for an NCBI database
##'
##' @param db An NCBI database
##'
##' @return A data frame
##'
##' @export
##' @examples
##'    pml <- listLinks("pubmed")
##'    pml$Name
listLinks <- function (db) {
  if (missing(db))
    stop("No database specified")
  if (!is.character(db))
    stop("db argument in no string. This argument should be a single character string")

  return(slot(einfo(db), "links"))
}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
