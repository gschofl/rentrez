### Eutil Classes ##########################################################
##' @include utils.r
##' @include blast-classes.r
NULL

#### Old Classes ####
setOldClass("list")
setOldClass("data.frame")

#### Class Unions ####
setClassUnion("XMLOrChar", c("XMLInternalDocument", "character"))
setClassUnion("ListOrFrame", c("data.frame", "list"))
setClassUnion("characterOrNull", c("character", "NULL"))

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
.eutil <- 
  setClass("eutil",
           representation(url = "character",
                          error = "list",
                          data = "XMLOrChar"),
           prototype(url = NA_character_,
                     error = list(),
                     data = NA_character_))

##' @export
setMethod("$", "eutil",
          function (x, name) x@name)

##' @export
setMethod("names", "eutil",
          function (x) slotNames(x))

##' Container for UIDs and the name of their database
##' 
##' @keywords internal
.idlist <-
  setClass("idlist",
           representation(database = "character",
                          queryKey = "integer",
                          webEnv = "character",
                          idList = "character"),
           prototype(database = NA_character_,
                     queryKey = NA_integer_,
                     webEnv = NA_character_,
                     idList = NA_character_))

##' @export
setMethod("show", "idlist",
          function (object) {
            cat(sprintf("List of UIDs from the %s database.\n",
                        sQuote(object@database)))
            print(object@idList)
            return(invisible(NULL))
          })

##' @export
setMethod("[", c("idlist", "numeric", "missing"),
          function (x, i) {
            .idlist(database = x@database,
                    queryKey = NA_integer_,
                    webEnv = NA_character_,
                    idList = x@idList[i])
          })


# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
