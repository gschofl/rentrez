#' @include utils.r
#' @include eutil.r
NULL


# webenvOrUid ------------------------------------------------------------


#' @rdname uidList
#' @export
setClass("uidList",
         representation(database = "character",
                        retmax = "numeric",
                        retstart = "numeric",
                        count = "numeric",
                        queryTranslation = "character"),
         prototype(database = NA_character_,
                   retmax = NA_integer_,
                   retstart = NA_integer_,
                   count = NA_integer_,
                   queryTranslation = NA_character_))

#' @rdname uidList
#' @export
setClass("uid",
         representation(uid = "character"),
         prototype(uid = NA_character_),
         contains="uidList")

#' @rdname uidList
#' @export
setClass("webenv",
         representation(queryKey = "integer",
                        webEnv = "character"),
         prototype(queryKey = NA_integer_,
                   webEnv = NA_character_),
         contains="uidList")


#' A list of Entrez UIDs that match a text query
#' 
#' @slot database Database from which the UIDs were retrieved.
#' @slot retstart The index of the first UID that is returned.
#' @slot retmax The number of UIDs out of the total number of
#' records that is returned.
#' @slot count The total number of records matching a query.
#' @slot queryTranslation The search term as translated by the
#' Entrez search system.
#' @slot uid A list of primary UIDs or a Query Key and Web Environment
#' string.
#' @slot queryKey
#' @slot webEnv
#' 
#' @rdname uidList
#' @export
#' @dev
#' @classHierarchy
#' @classMethods
setClassUnion("webenvOrUid", c("webenv", "uid"))


# accessor methods -------------------------------------------------------


setMethod("database", "webenvOrUid", function(x) x@database)

setMethod("retmax", "webenvOrUid", function(x) x@retmax)

setMethod("retstart", "webenvOrUid", function(x) x@retstart)

setMethod("count", "webenvOrUid", function(x) x@count)

setMethod("queryTranslation", "webenvOrUid", function(x) x@queryTranslation)

setMethod("uid", "webenvOrUid", function(x) {
  if (is(x, "uid")) {
    x@uid
  } else if (is(x, "webenv")) {
    message("No UIDs available. Use 'queryKey' and 'webEnv'")
    return(invisible(NULL))
  }
})

setMethod("webEnv", "webenvOrUid", function(x) {
  if (is(x, "webenv")) {
    x@webEnv
  } else if (is(x, "uid")) {
    message("No Web Environment string available. Use 'uid'")
    return(invisible(NULL))
  }
})

setMethod("queryKey", "webenvOrUid", function(x) {
  if (is(x, "webenv")) {
    x@queryKey
  } else if (is(x, "uid"))  {
    message("No Query Key available. Use 'uid'")
    return(invisible(NULL))
  }
})


# length method ----------------------------------------------------------


setMethod("length", "webenvOrUid", function (x) {
  if (is(x, "uid")) {
    length(uid(x))
  } else {
    count(x)
  }
})


# subsetting method ------------------------------------------------------


setMethod("[", c("webenvOrUid", "numeric"),
          function (x, i, j, ..., drop = TRUE) {
            if (is(x, "uid")) {
              uids <- uid(x)[i]
              new("uid", database = database(x), retmax = length(uids),
                  retstart = retstart(x), count = count(x),
                  queryTranslation = queryTranslation(x), uid = uids)
            } else if (is(x, "webenv")) {
              message("No subsetting for ", sQuote(class(x)),  " objects.")
              return(x)
            }
          })


# show methods ------------------------------------------------------------


setMethod("show", "webenvOrUid",
          function (object) {
            if (is(object, "uid")) {
              cat(sprintf("List of UIDs from the %s database.\n",
                          sQuote(database(object))))
              print(uid(object))
            } else if (is(object, "webenv")) {
              cat(sprintf("Web Environment for the %s database.\n",
                          sQuote(database(object))))
              cat(sprintf("Number of UIDs stored on the History server: %s\n",
                          count(object)))
              cat(sprintf("Query Key: %s\nWebEnv: %s\n",
                          queryKey(object), webEnv(object)))
            }
            invisible(NULL)
          })

