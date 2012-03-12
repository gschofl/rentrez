### Efetch #################################################################
##' @include eutil.r
##' @include utils.r
NULL

##' efetch class
##' 
##' efetch is an S4 class that extends the \code{\link{eutil-class}}.
##' This class provides a container for data retrived by calls to the 
##' NCBI EFetch utility.
##' 
##' efetch objects have three slots in addition to the slots provided by
##' basic \code{\link{eutil-class}} objects:
##' \describe{
##'   \item{database}{The name of the queried database.}
##'   \item{type}{A character vector specifying the record view returned,
##'   such as Abstract or MEDLINE from PubMed, or GenPept or FASTA from 
##'   protein}
##'   \item{mode}{A character vector specifying the data format of the
##'   records returned, such as plain text, HMTL or XML}
##' }
##' 
##' @seealso \code{\link{efetch}} for generating calls to the NCBI EFetch
##' utility.
##' 
##' @name efetch-class
##' @rdname efetch-class
##' @exportClass efetch
##' @aliases show,efetch-method
##' @aliases efetch,efetch-method
setClass("efetch", 
         representation(database = "character",
                        type = "character",
                        mode = "character"),
         prototype(database = NA_character_,
                   type = NA_character_,
                   mode = NA_character_),
         contains = "eutil")

##' @export
setMethod("show",
          signature(object = "efetch"),
          function (object) {
            cat(sprintf("EFetch query using the %s database.\nQuery url: %s\n\n",
                        sQuote(object@database), sQuote(object@url)))
            cat(object@data)
            return(invisible(NULL))
          })

##' Retrieve data records in the requested format from NCBI
##'
##' \code{efetch} retrieves data records in the requested format from a 
##' character vector of one or more primary UIDs or from a set of UIDs
##' stored in the user's web environment.
##' 
##' See the online documentation at
##' \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}
##' for additional information.
##' 
##' @param id (Required)
##' List of UIDs provided either as a character vector, as an
##' \code{\link{esearch-class}} object, or by reference to a web environment
##' and a query key obtained directly from the objects returned by previous
##' \code{\link{esearch}} (if \code{usehistory} was set \code{TRUE}),
##' \code{\link{epost}} or \code{\link{elink}} calls.
##' If UIDs are provided as a plain character vector, \code{db} must be
##' specified explicitly, and all of the UIDs must be from the database
##' specified by \code{db}.
##' @param db (Required only when \code{id} is a character vector of UIDs)
##' Database from which to retrieve records.
##' @param query_key An integer specifying which of the UID lists attached
##' to a user's Web Environment will be used as input to \code{efetch}.
##' (Usually obtained drectely from objects returned by previous
##' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}} calls.)
##' @param WebEnv A character string specifying the Web Environment that
##' contains the UID list. (Usually obtained directely from objects returned
##' by previous \code{\link{esearch}}, \code{\link{epost}} or
##' \code{\link{elink}} calls.)
##' @param rettype A character string specifying the data format of the
##' records returned, such as plain text, HMTL or XML. See 
##' \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
##' for allowed values for each database.
##' @param retmode A character string specifying the record view returned,
##' such as Abstract or MEDLINE from PubMed, or GenPept or FASTA from protein.
##' See \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
##' for allowed values for each database.
##' @param retstart Numeric index of the first record to be retrieved.
##' @param retmax Total number of records from the input set to be retrieved.
##' @param strand Strand of DNA to retrieve. (1: plus strand, 2: minus strand)
##' @param seq_start First sequence base to retrieve.
##' @param seq_stop Last sequence base to retrieve.
##' @param complexity Data content to return. (0: entire data structure,
##' 1: bioseq, 2: minimal bioseq-set, 3: minimal nuc-prot, 4: minimal pub-set)
##' 
##' @return An \code{\link{efetch-class}} object.
##'
##' @export
##' @examples
##'   # Fetch PMIDs 17284678 and 9997 as text abstracts
##'   efetch(c(17284678,9997), "pubmed", retmode="text", rettype="abstract")
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
    stop("No UIDs provided")
  if (is.null(db))
    stop("No database name provided")
  
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
  else if (is(id, "esearch") || is(id, "epost") || is(id, "elink")) {
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
    o <- .query("efetch", db=db, id=collapseUIDs(id),
                retmode=retmode, rettype=rettype, retstart=retstart,
                retmax=retmax, strand=strand, seq_start=seq_start,
                seq_stop=seq_stop, complexity=complexity)
  }

  new("efetch", url=o@url, data=o@data, database=db,
      mode=retmode, type=rettype)
}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

