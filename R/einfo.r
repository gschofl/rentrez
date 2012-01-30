##' Retrieves information about NCBI's databases
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
    o <- .query(eutil = 'einfo')
    new("einfoDbList", url = o@url, xml = o@xml,
        dbList = as.character(xpathSApply(o@xml, '//DbList/DbName', xmlValue)))
  }
  else {
    if (length(db) > 1) {
      warning("Only the first database name will be queried")
      db <- db[1]
    }
    o <- .query(eutil = 'einfo', db = db)

    # extract FieldList elements
    fnm <- sapply(getNodeSet(o@xml, '//FieldList/Field[1]/child::node( )'), xmlName)
    if (length(fnm) > 0)
      field_info <- as.data.frame(stringsAsFactors = FALSE,
                                  split(sapply(getNodeSet(o@xml, '//FieldList/Field/*'),
                                               xmlValue), fnm))[, fnm]
    else
      field_info <- data.frame()

    # extract LinkList elements
    lnm <- sapply(getNodeSet(o@xml, '//LinkList/Link[1]/child::node( )'), xmlName)
    if (length(lnm) > 0)
      link_info <- as.data.frame(stringsAsFactors = FALSE,
                                 split(sapply(getNodeSet(o@xml, '//LinkList/Link/*'),
                                              xmlValue), lnm))[, lnm]
    else
      link_info <- data.frame()

    new("einfoDb", url=slot(o, "url"), xml=slot(o, "xml"),
        error=checkErrors(o),
        dbName=xmlValue(xmlRoot(o@xml)[[1]][['DbName']]),
        menuName=xmlValue(xmlRoot(o@xml)[[1]][['MenuName']]),
        description=xmlValue(xmlRoot(o@xml)[[1]][['Description']]),
        records=as.numeric(xmlValue(xmlRoot(o@xml)[[1]][['Count']])),
        lastUpdate=as.POSIXlt(xmlValue(xmlRoot(o@xml)[[1]][['LastUpdate']])),
        fields=field_info,
        links=link_info)
  }
}

###### Accessor for einfo
.get.einfo <- function(obj, what) {
  if (is(obj, "einfoDbList"))
    switch(what,
           all=obj,
           default=slot(obj, "dbList"),
           slot(obj, what))
  else if (is(obj, "einfoDb"))
    switch(what,
           all=obj,
           default=list(dbName = slot(obj, "dbName"),
                        menuName = slot(obj, "menuName"),
                        description = slot(obj, "description"),
                        records = slot(obj, "records"),
                        lastUpdate = slot(obj, "lastUpdate"),
                        fields = slot(obj, "fields"),
                        links = slot(obj, "links")),
           slot(obj, what))
}

###### Convenience functions

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
    stop("db argument in no string. This argument should be a single
         character string")

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
    stop("db argument in no string. This argument should be a single
         character string")

  return(slot(einfo(db), "links"))
}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
