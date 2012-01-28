###### Accessor Methods ####################################################

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
##' @rdname getContent-methods
##' @docType methods
##' @keywords internal
##' @examples
##'    ## examples
setGeneric(".get", function (object, what = "default") {
  standardGeneric(".get")
  })

##' @rdname getContent-methods
##' @aliases getContent,einfo-method,eutil-method
setMethod(".get", signature("einfo"), function (object, what) {
  .get.einfo(object, what)
  })

##' @rdname getContent-methods
##' @aliases getContent,esearch-method,eutil-method
setMethod("getContent", signature("esearch"), function (object, what) {
  .get.esearch(object, what)
  })

##' @rdname getContent-methods
##' @aliases getContent,esummary-method,eutil-method
setMethod("getContent", signature("esummary"), function (object, what) {
  .get.esummary(object, what)
  })

###### Extract Methods #####################################################

##' Extact slots of \code{eutil-class} objects
##'
##' @rdname extract-methods
##' @aliases $,eutil-method
##' @docType methods
##' @keywords internal
setMethod("$", signature("eutil"), function (x, name) {
  return(slot(x, name))
})


##' @rdname extract-methods
##' @aliases $,esummary-method
setMethod("$", signature("esummary"), function (x, name) {
  if (!is.null(slot(x, "documentSummary")) && !identical(name, "documentSummary"))
    return(slot(x, "documentSummary")[[name, exact = FALSE]])
  else
    return(slot(x, name))
})


##' Extract elements of \code{einfo} class database lists
##'
##' @rdname extract-methods
##' @aliases [,einfoDbList-method
setMethod("[",
          signature(x = "einfoDbList", i = "numeric"),
          function (x, i, j, ..., drop) {
              slot(x, "dbList")[i]
          })

##' Extract elements of \code{esearch-class} uid lists
##'
##' @rdname extract-methods
##' @aliases [,esearch-method
setMethod("[",
          signature(x = "esearch", i = "numeric"),
          function (x, i, j, ..., drop = FALSE) {
            new("esearch",
                database = slot(x, "database"),
                idList = slot(x, "idList")[i])
          })


##' Extract elements of \code{esummary} objects
##'
##' @rdname extract-methods
##' @aliases [,esummary-method
setMethod("[",
          signature(x = "esummary", i = "ANY"),
          function (x, i, j, ..., drop = FALSE) {
            if (is(i, "esearch")) {
              return(slot(x, "documentSummary")[slot(i, "idList")])
            }
            return(slot(x, "documentSummary")[i])
          })


###### Names Methods #######################################################

##' Access slots names of \code{eutil-class} objects
##'
##' @name names
##' @rdname names-methods
##' @aliases names,eutil-method
##' @docType methods
##' @keywords internal
setMethod("names", signature("eutil"), function (x) {
  return(slotNames(x))
  })


###### Length Methods ######################################################

##' Get the length of an \code{esearch} object
##'
##' @name length
##' @rdname length-methods
##' @aliases length,esearch-method
##' @docType methods
##' @keywords internal
setMethod("length", signature("esearch"), function (x) {
  return(length(slot(x, "idList")))
  })


###### Show methods ########################################################

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
setMethod("show", signature("einfo"),
  function (object) {
    if (is(object, "einfoDbList")) {
      cat("List of all valid Entrez databases\n")
      print(.get(object))
      # return(invisible(.get(object)))
      return(invisible(NULL))
    }
    if (is(object, "einfoDb")) {
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
      # return(invisible(.get(object)))
      return(invisible(NULL))
    }
  })

print.einfo <- show


##' @rdname show-methods
##' @aliases show,esearch-method
setMethod("show", signature("esearch"),
  function (object) {
    if (is.null(slot(object, "count"))) {
      cat(paste("Database: '", slot(object, "database"), "'\n", sep = ""))
      print(slot(object, "idList"))
      return(invisible(NULL))
    }
    else {
      cat(paste("ESearch query using the \'", slot(object, "database"),
                "\' database.\nQuery term: ", slot(object, "queryTranslation"),
                "\nNumber of hits: ", slot(object, "count"),
                "\n", sep = ""))
      print(slot(object, "idList"))
      return(invisible(NULL))
    }
  })

print.esearch <- show


##' @rdname show-methods
##' @aliases show,esummary-method
setMethod("show", signature("esummary"),
  function(object) {
    cat(paste("Esummary query using the \'", 
              slot(object, "database"), "\' database.",
              "\n$ documentSummary : ", sep=""))
    if (is.null(slot(object, "documentSummary")))
      print(slot(object, "xml"))
    else
      print(str(slot(object, "documentSummary")))
    return(invisible(NULL))
  })

print.esearch <- show


# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:

