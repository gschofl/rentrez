### accessor methods #######################################################

##' Get content from an \code{eutil}-class object
##'
##' This is internal and not terribly useful to others
##'
##' @param object An object of class \code{eutil}.
##'
##' @param what A character string. "default" returns the parsed content in
##'   a vector or as a list. Any valid slot name returns the content of the 
##'   slot.
##'
##' @return A vector or a list
##'
##' @rdname .get-methods
##' @docType methods
##' @keywords internal
##' @examples
##'    ## examples
setGeneric(".get", function (x, what="default") {
  standardGeneric(".get")
  })


### extract methods ########################################################

##' Extact slots of \code{eutil-class} objects
##'
##' @rdname extract-methods
##' @aliases $,eutil-method
##' @docType methods
##' @keywords internal
setMethod("$",
          signature(x="eutil"),
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
          signature(x="eutil"),
          function (x) {
            return(slotNames(x))
          })


### length methods #########################################################

##' Get the length of an \code{esearch} object
##'
##' @name length
##' @rdname length-methods
##' @aliases length,esearch-method
##' @docType methods
##' @keywords internal
setMethod("length",
          signature(x="esearch"),
          function (x) {
            return(length(slot(x, "idList")))
          })

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:

