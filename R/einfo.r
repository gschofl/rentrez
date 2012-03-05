### Einfo ##################################################################

## Virtual class "einfo"
setClass("einfo", representation("VIRTUAL"), contains = "eutil")


## Class "einfoDbList"
setClass("einfoDbList",
         representation(dbList = "charOrNULL"),
         prototype(dbList = NULL),
         contains = "einfo")


## class "einfoDb"
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


##' Retrieve information about NCBI's databases
##'
##' If no database is provided \code{einfo} will return a list of databases
##' available for querying. For specific databases, \code{einfo} provides
##' the name, a description, the number indexed in the database, the date
##' of the last update of the database, the fields and the available links
##' to other Entrez databases.
##'
##' @param db \code{NULL} or a valid NCBI database name
##'
##' @return ReturnValue
##'
##' @seealso linkToSomewhere
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
    if (length(db) > 1) {
      warning("Only the first database name will be queried")
      db <- db[1]
    }
    o <- .query(eutil='einfo', db=db)

    # extract FieldList elements
    fnm <- sapply(getNodeSet(o@data, '//FieldList/Field[1]/child::node( )'), xmlName)
    if (length(fnm) > 0)
      field_info <- as.data.frame(stringsAsFactors = FALSE,
                                  split(sapply(getNodeSet(o@data, '//FieldList/Field/*'),
                                               xmlValue), fnm))[, fnm]
    else
      field_info <- data.frame()

    # extract LinkList elements
    lnm <- sapply(getNodeSet(o@data, '//LinkList/Link[1]/child::node( )'), xmlName)
    if (length(lnm) > 0)
      link_info <- as.data.frame(stringsAsFactors = FALSE,
                                 split(sapply(getNodeSet(o@data, '//LinkList/Link/*'),
                                              xmlValue), lnm))[, lnm]
    else
      link_info <- data.frame()

    new("einfoDb", url=o@url, data=o@data,
        error=checkErrors(o),
        dbName=xmlValue(xmlRoot(o@data)[[1]][['DbName']]),
        menuName=xmlValue(xmlRoot(o@data)[[1]][['MenuName']]),
        description=xmlValue(xmlRoot(o@data)[[1]][['Description']]),
        records=as.numeric(xmlValue(xmlRoot(o@data)[[1]][['Count']])),
        lastUpdate=as.POSIXlt(xmlValue(xmlRoot(o@data)[[1]][['LastUpdate']])),
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

### accessor methods #######################################################

.get.einfo <- function(x, what) {
  if (is(x, "einfoDbList"))
    switch(what,
           all=x,
           default=x@dbList,
           slot(x, what))
  else if (is(x, "einfoDb"))
    switch(what,
           all=x,
           default=list(dbName = slot(x, "dbName"),
                        menuName = slot(x, "menuName"),
                        description = slot(x, "description"),
                        records = slot(x, "records"),
                        lastUpdate = slot(x, "lastUpdate"),
                        fields = slot(x, "fields"),
                        links = slot(x, "links")),
           slot(x, what))
}

##' @rdname .get-methods
##' @aliases .get,einfo-method,eutil-method
setMethod(".get",
          signature(x="einfo"),
          function (x, what) {
            .get.einfo(x, what)
          })

### extract methods ########################################################

##' Extract elements of \code{einfo} class database lists
##'
##' @rdname extract-methods
##' @aliases [,einfoDbList-method
setMethod("[",
          signature(x="einfoDbList", i="numeric"),
            function (x, i, j, ..., drop) {
              x@dbList[i]
            })


### show methods ###########################################################

##' method extension to show for eutil classes
##'
##' @return NULL
##'
##' @name show
##' @aliases show,einfo-method
##' @rdname show-methods
##' @docType methods
##' @export
##'
##' @seealso \code{\link[methods]{show}}
##' @examples
##' # print(einfo())
##' # einfo("snp")
setMethod("show",
          signature(object="einfo"),
          function (object) {
            if (is(object, "einfoDbList")) {
              cat("List of all valid Entrez databases\n")
              print(.get(object))
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

print.einfo <- show

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
