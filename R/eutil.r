#### eutil class definitions ###############################################

## Class Unions
setClassUnion("listOrNULL", c("list","NULL"))
setClassUnion("dfOrNULL", c("data.frame","NULL"))
setClassUnion("XMLOrChar", c("XMLInternalDocument","character"))
setClassUnion("charOrNULL", c("character","NULL"))
setClassUnion("numOrNULL", c("numeric","NULL"))
setClassUnion("POSOrNULL", c("POSIXlt","NULL"))

##' eutil class
##'
##' eutil is an S4 class that is extended by the 
##' \code{\link{einfo-class}}, \code{\link{esearch-class}},
##' \code{\link{esummary-class}}, \code{\link{efetch-class}}, and
##' \code{\link{epost-class}} classes.
##' 
##' These classes provide containers for the data returned from calls to the
##' NCBI Entrez Utilities. 
##' Detailed information about the services provided by NCBI is available
##' at \url{http://www.ncbi.nlm.nih.gov/books/NBK25501/}
##'
##' Eutil class objects have three slots:
##' \describe{
##'   \item{url}{A character vector containing the URL submitted to Entrez.}
##'   \item{error}{Any error messages parsed from the output of the
##'   call submitted to Entrez.}
##'   \item{data}{An \code{\link[XML]{XMLInternalDocument-class}} or
##'   character vector holding the unparsed output from the call submitted
##'   to Entrez.}
##' }
##' 
##' @name eutil-class
##' @rdname eutil-class
##' @exportClass eutil
##' @aliases eutil,eutil-method
setClass("eutil",
         representation(url = "character",
                        error= "listOrNULL",
                        data = "XMLOrChar"),
         prototype(url = character(),
                   error = NULL,
                   data = ""))

#### eutil method definitions ##############################################

### extract methods ########################################################

##' Extact slots of \code{eutil-class} objects
##'
##' @rdname extract-methods
##' @aliases $,eutil-method
##' @docType methods
##' @keywords internal
setMethod("$",
          signature(x = "eutil"),
          function (x, name) {
            return(slot(x, name))
          })


### names methods ##########################################################

##' Access slots names of \code{eutil-class} objects
##'
##' @name names
##' @rdname names-methods
##' @aliases names,eutil-method
##' @docType methods
##' @keywords internal
setMethod("names",
          signature(x = "eutil"),
          function (x) {
            return(slotNames(x))
          })




# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76: