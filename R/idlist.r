#' @include utils.r
#' @include eutil.r
NULL


# idList ------------------------------------------------------------


#' A list of Entrez UIDs that match a text query
#' 
#' @slot database Database from which the UIDs were retrieved.
#' @slot retstart The index of the first UID that is returned.
#' @slot retmax The number of UIDs out of the total number of
#' records that is returned.
#' @slot count The total number of records matching a query.
#' @slot queryTranslation The search term as translated by the
#' Entrez search system.
#' @slot queryKey
#' @slot webEnv
#' @slot idList A list of primary UIDs.
#' 
#' @classHierarchy
#' @classMethods
setClass("idList",
         representation(database = "character",
                        retmax = "numeric",
                        retstart = "numeric",
                        count = "numeric",
                        queryTranslation = "character",
                        queryKey = "numeric",
                        webEnv = "character",
                        idList = "character"),
         prototype(database = NA_character_,
                   retmax = NA_integer_,
                   retstart = NA_integer_,
                   count = NA_integer_,
                   queryTranslation = NA_character_,
                   queryKey = NA_integer_,
                   webEnv = NA_character_,
                   idList = NA_character_))

# accessor methods -------------------------------------------------------


setMethod("database", "idList", function(x) x@database)

setMethod("retmax", "idList", function(x) x@retmax)

setMethod("retstart", "idList", function(x) x@retstart)

setMethod("count", "idList", function(x) x@count)

setMethod("queryTranslation", "idList", function(x) x@queryTranslation)

setMethod("queryKey", "idList", function(x) x@queryKey)

setMethod("webEnv", "idList", function(x) x@webEnv)

setMethod("idList", "idList", function(x, db = TRUE) {
  if (db) {
    structure(x@idList, database = database(x))
  } else {
    x@idList
  }
})


# length method ----------------------------------------------------------


setMethod("length", "idList", function (x) {
  if (has_webenv(x)) {
    retmax(x)
  } else {
    count(x)
  }
})


# subsetting method ------------------------------------------------------


#' @autoImports
setMethod("[", c("idList", "numeric"),
          function (x, i, j, ..., drop = TRUE) {
            ids <- x@idList[i]
            initialize(.Object=x, retmax = length(compactNA(ids)),
                       retstart = retstart(x) + i[1L] - 1L,
                       idList = ids)
          })


# show methods ------------------------------------------------------------


setMethod("show", "idList",
          function (object) {
            if (has_webenv(object)) {
              cat(sprintf("Web Environment for the %s database.\n",
                          sQuote(database(object))))
              cat(sprintf("Number of UIDs stored on the History server: %s\n",
                          count(object)))
              cat(sprintf("Query Key: %s\nWebEnv: %s\n",
                          queryKey(object), webEnv(object)))
            } else {
              cat(sprintf("List of UIDs from the %s database.\n",
                          sQuote(database(object))))
              print(idList(object))
            }
            invisible(NULL)
          })

