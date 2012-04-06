#### eutil class definitions ###############################################
setOldClass("list")
setOldClass("data.frame")

## Class Union
setClassUnion("XMLOrChar", c("XMLInternalDocument","character"))
setClassUnion("ListOrFrame", c("data.frame", "list"))

##' eutil class
##'
##' eutil is an S4 class that is extended by the 
##' \code{\link{einfo-class}}, \code{\link{esearch-class}},
##' \code{\link{esummary-class}}, \code{\link{efetch-class}}, and
##' \code{\link{epost-class}} classes.
##' 
##' These classes provide containers for the data returned from calls to the
##' NCBI Entrez Utilities.
##' Detailed information about the EUtilities provided by NCBI is available
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25501/}{here}.
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
##' @aliases $,eutil-method
##' @aliases names,eutil-method
setClass("eutil",
         representation(url = "character",
                        error = "list",
                        data = "XMLOrChar"),
         prototype(url = NA_character_,
                   error = list(),
                   data = NA_character_))

##' @export
setMethod("$",
          signature(x = "eutil"),
          function (x, name) {
            return(slot(x, name))
          })


##' @export
setMethod("names",
          signature(x = "eutil"),
          function (x) {
            return(slotNames(x))
          })


# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
