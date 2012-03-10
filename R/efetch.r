### Efetch #################################################################

## class "efetch"
setClass("efetch", 
         representation(database = "charOrNULL",
                        type = "charOrNULL",
                        mode = "charOrNULL"),
         contains = "eutil")

##' Retrieves data from NCBI's databases
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
##'   ## Not run: efetch("1234")
efetch <- function (id,
                    db=attr(id, "database"),
                    query_key=NULL,
                    WebEnv=NULL,
                    rettype=NULL,
                    retmode=NULL,
                    retstart=NULL,
                    retmax=NULL,
                    strand=NULL,
                    seq_start=NULL,
                    seq_stop=NULL,
                    complexity=NULL) {
  if (missing(id) && is.null(query_key) && is.null(WebEnv))
    stop("No UID(s) provided")
  if (is.null(db)) stop("No database name provided")
  
  ## setting default rettype and retmode for each database
  if (is.null(rettype)) {
    rettype <- switch(db,
                      nucleotide="gbwithparts",
                      nuccore="gbwithparts",
                      protein="gp")
  }
  
  if (is.null(retmode)) {
    retmode <- switch(db,
                      pubmed="xml",
                      nucleotide="text",
                      nuccore="text",
                      protein="text")
  }
  
  hasRes <- FALSE
  ## use WebEnv and QueryKey if available
  if (!is.null(query_key) && !is.null(WebEnv)) {
    o <- .query("efetch", db=db, query_key=query_key, WebEnv=WebEnv,
                retmode=retmode, rettype=rettype, retstart=retstart,
                retmax=retmax, strand=strand, seq_start=seq_start,
                seq_stop=seq_stop, complexity=complexity)
    hasRes <- TRUE
  }
  else if (is(id, "esearch")) {
    if (!is.na(id@queryKey) && !is.na(id@webEnv)) {
      o <- .query("efetch", db=db, query_key=id@queryKey, WebEnv=id@webEnv,
                  retmode=retmode, rettype=rettype, retstart=retstart,
                  retmax=retmax, strand=strand, seq_start=seq_start,
                  seq_stop=seq_stop, complexity=complexity)
      hasRes <- TRUE
    }
    else {
      id <- slot(id, "idList")
    }
  }
  
  if (!hasRes) {
    if (length(id) > 1) {
      if (length(id) > 200) {
        warning("The UID list is too large. Only the first 200 UIDs will be used")
        id <- id[1:200]
      }
      id <- paste(id, collapse = ",")
    }
    
    o <- .query("efetch", db=db, id=id,
                retmode=retmode, rettype=rettype, retstart=retstart,
                retmax=retmax, strand=strand, seq_start=seq_start,
                seq_stop=seq_stop, complexity=complexity)
  }

  new("efetch", url=o@url, data=o@data, database=db,
      mode=retmode, type=rettype)
}

##' @rdname show-methods
##' @aliases show,esearch-method
setMethod("show",
          signature(object="efetch"),
          function (object) {
            cat(sprintf("EFetch query using the %s database.\nQuery url: %s\n\n",
                        sQuote(object@database), sQuote(object@url)))
            cat(object@data)
            return(invisible(NULL))
          })

print.esearch <- show


# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

