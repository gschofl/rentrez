#### reutils classes definitions. All classes are S4.

require(XML)
require(RCurl)
require(methods)

## Class Unions
setClassUnion("listOrNULL", c("list","NULL"))
setClassUnion("dfOrNULL", c("data.frame","NULL"))
setClassUnion("XMLOrNULL", c("XMLInternalDocument","NULL"))
setClassUnion("charOrNULL", c("character","NULL"))
setClassUnion("numOrNULL", c("numeric","NULL"))
setClassUnion("POSOrNULL", c("POSIXlt","NULL"))

## Class "eutil"
setClass("eutil",
         representation(url = "character",
                        error= "listOrNULL",
                        xml = "XMLOrNULL"),
         prototype(url = character(),
                   error = NULL,
                   xml = NULL))

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


##' reutils formal class (S4) holding ESearch query results
##'
##' \describe{
##'       \item{database}{character vector containing the name of the database}
##'       \item{count}{an integer giving the total number of hits for a database query}
##'       \item{retMax}{an integer giving the number of hits retrieved}
##'       \item{retStart}{an integer giving the index of the first hit retrieved}
##'       \item{queryKey}{not implemented}
##'       \item{webEnv}{not implemented}
##'       \item{queryTranslation}{character vector containing the search term as translated by the Entrez search system}
##'       \item{idList}{character vector containing the UIDs returned}
##'       \item{url}{character vector containig the URL submitted to Entrez}
##'       \item{xml}{XMLInternalDocument holding the unparsed search output}
##' }
##'
##' @seealso \code{\link{eutil}}
##' 
##' @name esearch-class
##' @rdname esearch-class
##' @exportClass esearch
##' 
##' @examples
##' getSlots("esearch")
setClass("esearch",
         representation(database = "charOrNULL",
                        count = "numOrNULL",
                        retMax = "numOrNULL",
                        retStart = "numOrNULL",
                        queryKey = "numOrNULL",
                        webEnv = "charOrNULL",
                        queryTranslation = "charOrNULL",
                        idList = "charOrNULL"),
       prototype(database = NULL, count = NULL, retMax = NULL,
                 retStart = NULL, queryKey = NULL,
                 webEnv = NULL, queryTranslation = NULL,
                 idList = NULL),
         contains = "eutil")


## class "epost"
## not implemented yet


## class "esummary"
setClass("esummary",
         representation(database = "charOrNULL",
                        documentSummary = "listOrNULL"),
         prototype(database = NULL, documentSummary = NULL),
         contains = "eutil")


## class "efetch"
setClass("efetch",
         representation(response = "function"),
         prototype(response = function (obj) readLines(obj)),
         contains = "eutil")

# }
# .initReutilsClasses()
# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:

