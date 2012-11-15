#' @title Eutil accessors
#' 
#' @description
#' Access the various slots of \linkS4class{eutil} instances.
#' 
#' @param x An \code{\linkS4class{eutil}} instance.
#' @param ... Further arguments passed on to methods. 
#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("error", function(x, ...) standardGeneric("error"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("queryUrl",  function(x, ...) standardGeneric("queryUrl"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("retmode", function(x, ...) standardGeneric("retmode"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("rettype", function(x, ...) standardGeneric("rettype"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("database", function(x, ...) standardGeneric("database"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("retmax", function(x, ...) standardGeneric("retmax"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("retstart", function(x, ...) standardGeneric("retstart"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("count", function(x, ...) standardGeneric("count"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("queryTranslation", function(x, ...) standardGeneric("queryTranslation"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("idList", function(x, ...) standardGeneric("idList"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("webEnv", function(x, ...) standardGeneric("webEnv"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("queryKey", function(x, ...) standardGeneric("queryKey"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("content", function(x, ...) standardGeneric("content"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("docsum", function(x, ...) standardGeneric("docsum"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("linkSet", function(x, ...) standardGeneric("linkSet"))


#' @title EInfo accessors
#' 
#' @description
#' Access the various slots of \linkS4class{einfo} instances.
#' 
#' @param x An \code{\linkS4class{einfo}} instance.
#' @param ... Further arguments passed on to methods. 
#' @rdname einfo-accessors
#' @export
#' @genericMethods
setGeneric("dbName", function(x, ...) standardGeneric("dbName"))

#' @rdname einfo-accessors
#' @export
#' @genericMethods
setGeneric("menuName", function(x, ...) standardGeneric("menuName"))

#' @rdname einfo-accessors
#' @export
#' @genericMethods
setGeneric("description", function(x, ...) standardGeneric("description"))

#' @rdname einfo-accessors
#' @export
#' @genericMethods
setGeneric("records", function(x, ...) standardGeneric("records"))

#' @rdname einfo-accessors
#' @export
#' @genericMethods
setGeneric("lastUpdate", function(x, ...) standardGeneric("lastUpdate"))

#' @rdname einfo-accessors
#' @export
#' @genericMethods
setGeneric("fields", function(x, ...) standardGeneric("fields"))

#' @rdname einfo-accessors
#' @export
#' @genericMethods
setGeneric("links", function(x, ...) standardGeneric("links"))




