##' @include utils.r
##' @include eutil.r
NULL


# efetch-class -----------------------------------------------------------


##' \dQuote{efetch} class
##' 
##' efetch is an S4 class that provides a container for data retrived by calls
##' to the NCBI EFetch utility.
##' 
##' @section Slots:
##' \describe{
##'   \item{\code{url}:}{See \code{\linkS4class{eutil}}.}
##'   \item{\code{error}:}{See \code{\linkS4class{eutil}}.}
##'   \item{\code{content}:}{See \code{\linkS4class{eutil}}.}
##'   \item{\code{database}:}{The name of the queried database.}
##'   \item{\code{type}:}{A character vector specifying the record view
##'   returned, such as Abstract or MEDLINE from \sQuote{PubMed}, or GenPept
##'   or FASTA from \sQuote{protein}}
##'   \item{\code{mode}:}{A character vector specifying the data format of the
##'   records returned, such as plain text, HMTL or XML}
##' }
##' 
##' @section Extends: 
##'   Class \code{"\linkS4class{eutil}"}, directly.
##'   
##' @param ... arguments passed to the constructor method
##' 
##' @seealso \code{\link{efetch}} for generating calls to the NCBI EFetch
##' utility.
##' 
##' @name efetch-class
##' @rdname efetch-class
##' @exportClass efetch
##' @aliases content,efetch-method
##' @aliases write,efetch-method
##' @aliases c,efetch-method
.efetch <-
  setClass("efetch", 
           representation(database = "character",
                          type = "characterOrNull",
                          mode = "characterOrNull"),
           prototype(database = NA_character_,
                     type = NA_character_,
                     mode = NA_character_),
           contains = "eutil")


# show-method ------------------------------------------------------------


##' @aliases show,efetch-method
##' @rdname show-methods
setMethod("show", "efetch",
          function (object) {
            cat(sprintf("EFetch query using the %s database.\nQuery url: %s\n\n",
                        sQuote(object@database), sQuote(object@url)))
            cat(object@content)
            invisible()
          })


# write-method -----------------------------------------------------------


##' Write efetch data to file
##'
##' @usage write(x, file = "data", append = FALSE)
##' 
##' @param x An \code{efetch} object.
##' @param file A connection, or a character string naming the file to write to.
##' @param append if \code{TRUE} the data \code{x} is appended to the connection
##'
##' @export
##' @docType methods
##' @rdname write-methods
setMethod("write", "efetch",
          function (x, file = "data", append = FALSE) {
            write(x = x@content, file = file, append = append)
          })


# c-method ---------------------------------------------------------------


##' Combining efetch objects
##' 
##' Only data retrieved from the same datebase in the same \code{retmode} and
##' \code{rettype} can be combined.
##'
##' @usage c(...)
##' 
##' @param ... objects to be combined.
##'
##' @export
##' @docType methods
##' @rdname c-methods
setMethod("c", "efetch",
          function (x, ..., recursive = FALSE) {
            db <- unique(c(x@database, unlist(lapply(list(...), slot, "database"))))
            db <- db[-is.na(db)]
            if (length(db) > 1L)
              stop("Cannot combine objects from different databases")
            type <- unique(c(x@type, unlist(lapply(list(...), slot, "type"))))
            type <- type[-is.na(type)]
            if (length(type) > 1L)
              stop("Cannot combine objects with different data types")
            mode <- unique(c(x@mode, unlist(lapply(list(...), slot, "mode"))))
            mode <- mode[-is.na(mode)]
            if (length(mode) > 1L)
              stop("Cannot combine objects with different data modes")
            
            url <- c(x@url, unlist(lapply(list(...), slot, "url")))
            content <- c(x@content, unlist(lapply(list(...), slot, "content")))
            
            .efetch(url = url, content = content, error = list(),
                    database = db, mode = mode, type = type)
          })


# content-method ---------------------------------------------------------


##' @usage content(x, parse = TRUE, format = c("Biostrings", "DNAbin", "String"))
##'
##' @param format Output format of sequence data.
##' 
##' @rdname efetch-class
##' @rdname content-methods
setMethod("content", "efetch",
          function(x, parse = TRUE, ...) {
            if (isTRUE(parse)) {
              if (x@database == "pubmed") {
                return( .parsePubmed(x) )
              }
              if (x@database %in% c("protein","nucleotide","nuccore")) {
                return( .parseSequence(x, ...) )
              }
              return( x@content )

            } else {
              x@content
            }
          })


##' Retrieve data records in the requested format from NCBI
##'
##' \code{efetch} retrieves data records in the requested format from a 
##' character vector of one or more primary UIDs or from a set of UIDs
##' stored in the user's web environment.
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}{EUtilities}
##' for additional information.
##' 
##' The default retrieval mode (\code{retmode}) for the \code{pubmed},
##' \code{nuccore}, \code{protein}, and \code{gene} databases is 'text'. Default
##' \code{rettype}s are 'medline', 'gb', 'gp', and 'gene_table', respectively.
##' 
##' @param id (Required)
##' List of UIDs provided either as a character vector, as an
##' \code{\link{esearch-class}} object, or by reference to a web environment
##' and a query key obtained directly from previous calls to
##' \code{\link{esearch}} (if \code{usehistory} was set \code{TRUE}),
##' \code{\link{epost}} or \code{\link{elink}}.
##' If UIDs are provided as a plain character vector, \code{db} must be
##' specified explicitly, and all of the UIDs must be from the database
##' specified by \code{db}.
##' @param db (Required only when \code{id} is a character vector of UIDs)
##' Database from which to retrieve records. See
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/table/chapter2.chapter2_table1/?report=objectonly}{here}
##' for the supported databases.
##' @param query_key An integer specifying which of the UID lists attached
##' to a user's Web Environment will be used as input to \code{efetch}.
##' (Usually obtained drectely from objects returned by previous
##' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}} calls.)
##' @param WebEnv A character string specifying the Web Environment that
##' contains the UID list. (Usually obtained directely from objects returned
##' by previous \code{\link{esearch}}, \code{\link{epost}} or
##' \code{\link{elink}} calls.)
##' @param rettype A character string specifying the report type returned,
##' such as 'abstract' or 'medline' from PubMed, 'gp' or 'fasta' from
##' protein, or 'gb', 'gbwithparts, or 'fasta_cds_na' from nuccore.
##' See
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
##' for allowed values for each database.
##' @param retmode A character string specifying the data mode of the
##' records returned, such as plain text, XML, or asn.1. See 
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
##' for allowed values for each database.
##' @param retstart Numeric index of the first record to be retrieved.
##' @param retmax Total number of records from the input set to be retrieved.
##' @param strand Strand of DNA to retrieve. (1: plus strand, 2: minus strand)
##' @param seq_start First sequence base to retrieve.
##' @param seq_stop Last sequence base to retrieve.
##' @param complexity Data content to return. (0: entire data structure,
##' 1: bioseq, 2: minimal bioseq-set, 3: minimal nuc-prot, 4: minimal pub-set)
##' 
##' @return An \code{\linkS4class{efetch}} object.
##' @seealso \code{\link{efetch.batch}} for downloading more than about 500
##' data records.
##' \code{\link{content}} to retrieve data from \code{\linkS4class{efetch}}
##' objects.
##'
##' @export
##' @example inst/examples/efetch.r
efetch <- function (id, db = NULL, query_key = NULL, WebEnv = NULL,
                    rettype = NULL, retmode = NULL, retstart = NULL,
                    retmax = 500, strand = NULL, seq_start = NULL,
                    seq_stop = NULL, complexity = NULL) {
  
  ## id may be missing if WebEnv and query_key are provided
  if ((is.null(query_key) || is.null(WebEnv)) && missing(id)) {
    stop("No UIDs provided")
  }
  
  ## if WebEnv and query_key are provided, db must also be provided
  if (!is.null(query_key) && !is.null(WebEnv) && is.null(db)) {
    stop("No database name provided")
  }
  
  ## construct list of environment variables
  if (missing(id)) {
    ## if WebEnv and query_key is provided by the user set uid=NULL, count=0, 
    ## retmax stays restricted to 500.
    env_list <-list(WebEnv = WebEnv, query_key = query_key, count = 0,
                    uid = NULL, db = db)
  } else {
    env_list <-.getId(id)
    ## abort if no db was provided and id did not contain db 
    if (is.null(db) && is.null(db <- env_list$db)) {
      stop("No database name provided")
    }
  }

  ## set default rettype and retmode for some databases
  if (is.null(rettype)) {
    rettype <- switch(db, pubmed="medline", nucleotide="gb", nuccore="gb",
                      protein="gp", gene="gene_table")
  }
  
  if (is.null(retmode)) {
    retmode <- switch(db, pubmed="xml", nucleotide="text", nuccore="text",
                      protein="text", gene="text")
  }

  if (is.finite(env_list$count) && (env_list$count > 500 || retmax > 500)) {
    # if record_count exceeds 500 issue a warning and recommend
    # efetch.batch()
    message(gettextf("You are attempting to download %s records.\nOnly the first 500 are downloaded. Use efetch.batch() instead.",
                     max(c(env_list$count, retmax))))
    retmax <- 500
    env_list$uid <- env_list$uid[seq_len(500)]
  } else if (is.na(env_list$count)) {
    # this takes care of the cases where we don't actually know how many UIDs
    # are stored on the history server
    # message("A single download request is restricted to 500 records.\nUse efetch.batch() to download more records.")
    retmax <- 500
  }

  o <- if (length(env_list$uid) > 100) {
    # use HTTP POST if uploading more than 100 user provided UIDs.
    .httpPOST('efetch', db = db, id = .collapse(env_list$uid),
              query_key = env_list$query_key, WebEnv = env_list$WebEnv,
              retmode = retmode, rettype = rettype, retstart = as.character(retstart),
              retmax = as.character(retmax), strand = as.character(strand),
              seq_start = as.character(seq_start), seq_stop = as.character(seq_stop),
              complexity = as.character(complexity))
  } else {
    .query('efetch', db = db, id = .collapse(env_list$uid),
           query_key = env_list$query_key, WebEnv = env_list$WebEnv,
           retmode = retmode, rettype = rettype, retstart = retstart,
           retmax = retmax, strand = strand, seq_start = seq_start,
           seq_stop = seq_stop, complexity = complexity)
  }

  .efetch(url = o@url, content = o@content, error = list(),
          database = db, mode = retmode, type = rettype)
}

##' Retrieve batches of data records in the requested format from NCBI
##'
##' \code{efetch.batch} retrieves large data sets from NCBI in batches.
##' 
##' See the official online documentation for NCBI's
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}{EUtilities}
##' for additional information.
##' 
##' @param id (Required)
##' List of UIDs provided (via the Entrez History server) by an
##' \code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
##' \code{\linkS4class{elink}} object.
##' @param chunk_size Number of records downloaded as a batch (default: 200;
##' maximum: 500).
##' @param rettype A character string specifying the record view returned,
##' such as 'abstract' or 'medline' from PubMed, or 'gp' or 'fasta' from
##' protein.
##' See
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
##' for allowed values for each database.
##' @param retmode A character string specifying the data format of the
##' records returned, such as plain text, XML, or asn.1.
##' See
##' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
##' for allowed values for each database.
##' @param strand Strand of DNA to retrieve. (1: plus strand, 2: minus strand)
##' @param seq_start First sequence base to retrieve.
##' @param seq_stop Last sequence base to retrieve.
##' @param complexity Data content to return. (0: entire data structure,
##' 1: bioseq, 2: minimal bioseq-set, 3: minimal nuc-prot, 4: minimal pub-set)
##' 
##' @return An \code{\linkS4class{efetch}} object.
##'
##' @export
##' @example inst/examples/efetch.batch.r
efetch.batch <- function (id,
                          chunk_size=200,
                          rettype=NULL,
                          retmode=NULL,
                          strand=NULL,
                          seq_start=NULL,
                          seq_stop=NULL,
                          complexity=NULL) {
  
  if (!is(id, "esearch") && !is(id, "epost") && !is(id, "elink"))
    stop("efetch.batch() expects an 'esearch', 'epost', or 'elink' object")
  
  max_chunk <- 500
  if (chunk_size > max_chunk) {
    warning(sprintf("The maximum downloadable chunk size is %s."), 
            max_chunk, call.=FALSE)
    chunk_size <- max_chunk
  }
  
  if (id@count <= max_chunk) {
    res <- efetch(id=id, rettype=rettype, retmode=retmode, retstart=NULL,
                  retmax=NULL, strand=strand, seq_start=seq_start,
                  seq_stop=seq_stop, complexity=complexity)
  } else {
    n_chunks <- id@count %/% chunk_size
    retstart <- seq(from=1, to=n_chunks*chunk_size, by=chunk_size)
    res <- .efetch()
    for (start in retstart) {
      res <- c(res, efetch(id=id, rettype=rettype, retmode=retmode,
                           retstart=start, retmax=chunk_size, strand=strand,
                           seq_start=seq_start, seq_stop=seq_stop,
                           complexity=complexity))
      Sys.sleep(time=0.33)
    }
  }
  res
}

## convenience methods for bibentries ####

##' Open a bibentry in the browser
##' 
##' @param ref A \code{\link[utils]{bibentry}} object
##' @param ... Further arguments
##' 
##' @export
browse <- function (ref, ...) {
  UseMethod("browse", ref)
}

#' @S3method browse bibentry
browse.bibentry <- function (ref, browser = getOption("browser")) {  
  if (all(!nzchar(ref$doi))) {
    return("No doi available")
  }
  l <- lapply(ref$doi[nzchar(ref$doi)], function (doi) {
    browseURL(paste0('http://dx.doi.org/', doi), browser = browser)
  })
  invisible()
}

##' Access abstract from a bibentry
##' 
##' @param ref A \code{\link[utils]{bibentry}} object
##' @param ... Further arguments
##' 
##' @export
abstract <- function (ref, ...) {
  UseMethod("abstract", ref)
}

#' @S3method abstract bibentry
abstract.bibentry <- function (ref) {
  return(ref$abstract)
}


#' @S3method print idlist
print.idlist <- function (x) {
  print(unclass(x))
  invisible()
}


#' @S3method print webenv
print.webenv <- function (x) {
  print(unclass(x))
  invisible()
}


#' @S3method [ idlist
`[.idlist` <- function (x, i, j, ..., drop = TRUE) {
  v <- NextMethod()
  structure(v, database = attr(x, "database"), class = attr(x, "class"))
}



# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

