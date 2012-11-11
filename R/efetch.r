#' @include utils.r
#' @include eutil.r
NULL

# efetch-class -----------------------------------------------------------

#' efetch
#' 
#' \dQuote{efetch} is an S4 class that provides a container for data retrived
#' by calls to the NCBI EFetch utility.
#' 
#' @slot url A character vector containing the query URL.
#' @slot error Any error or warning messages parsed from
#' the output of the call submitted to Entrez.
#' @slot content An \code{\linkS4class{XMLInternalDocument}} object or
#' a character vector holding the unparsed output from the call
#' submitted to Entrez.
#' @slot database A character vector giving the name of the queried database.
#' @slot rettype Retrieval Mode. A character vector specifying the record
#' view returned, such as \sQuote{Abstract} or \sQuote{MEDLINE} from
#' \emph{pubmed}, or \sQuote{GenPept} or \sQuote{FASTA} from \emph{protein}.
#' @slot retmode Retrieval Mode. A character vector specifying the data format
#' of the records returned, such as plain \sQuote{text}, \sQuote{HMTL} or 
#' \sQuote{XML}.
#' 
#' @rdname efetch
#' @export
#' @classHierarchy
#' @classMethods
setClass("efetch", 
         representation(database = "character",
                        rettype = "characterOrNull",
                        retmode = "characterOrNull"),
         prototype(database = NA_character_,
                   rettype = NA_character_,
                   retmode = NA_character_),
         contains = "eutil")


# accessor-methods -------------------------------------------------------


setMethod("database", "efetch", function(x) x@database)

setMethod("retmode", "efetch", function(x) x@retmode)

setMethod("rettype", "efetch", function(x) x@rettype)


# show-method ------------------------------------------------------------


setMethod("show", "efetch",
          function (object) {
            cat(sprintf("EFetch query using the %s database.\nQuery url: %s\n\n",
                        sQuote(database(object)), sQuote(query(object))))
            cat(object@content)
            invisible(NULL)
          })


# write-method -----------------------------------------------------------


#' Write efetch data to file
#'
#' @param x An \code{\linkS4class{efetch}} instance.
#' @param file A connection, or a character string naming the file to write to.
#' @param append Append the data \code{x} to the connection.
#' @export
setMethod("write", "efetch",
          function (x, file = "data", append = FALSE) {
            write(x = x@content, file = file, append = append)
          })


# c-method ---------------------------------------------------------------


#' Combining efetch objects
#' 
#' Only data retrieved from the same datebase in the same \code{retmode} and
#' \code{rettype} can be combined.
#'
#' @usage c(...)
#' @param ... objects to be combined.
#' @export
#' @autoImports
setMethod("c", "efetch",
          function (x, ..., recursive = FALSE) {
            db <- compactNA(unique(c(database(x), vapply(list(...), database, character(1)))))
            if (length(db) > 1L)
              stop("Cannot combine objects from different databases")
            rt <- compactNA(unique(c(rettype(x), vapply(list(...), rettype, character(1)))))
            if (length(rt) > 1L)
              stop("Cannot combine objects with different data types")
            rm <- compactNA(unique(c(retmode(x), vapply(list(...), retmode, character(1)))))
            if (length(rm) > 1L)
              stop("Cannot combine objects with different data modes")
            
            url <- c(query(x), vapply(list(...), query, character(1)))
            content <- c(x@content, unlist(lapply(list(...), slot, "content")))
            
            new("efetch", url = url, content = content, error = list(),
                database = db, retmode = rm, rettype = rt)
          })


# content-method ---------------------------------------------------------


#' @autoImports
setMethod("content", "efetch",
          function(x, parse = TRUE, ...) {
            if (isTRUE(parse)) {
              if (database(x) == "pubmed") {
                return( .parsePubmed(x) )
              }
              if (database(x) == "taxonomy") {
                return( .parseTaxon(x) )
              }
              if (database(x) %in% c("protein","nucleotide","nuccore")) {
                return( .parseSequence(x, ...) )
              }
              return( x@content )

            } else {
              x@content
            }
          })


#' \code{efetch} retrieves data records in the requested format from a
#' character vector of one or more primary UIDs or from a set of UIDs stored 
#' in the user's web environment.
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}{EUtilities}
#' for additional information.
#' 
#' The default retrieval mode (\code{retmode}) for the \code{pubmed},
#' \code{nuccore}, \code{protein}, and \code{gene} databases is 'text'. Default
#' \code{rettype}s are 'medline', 'gb', 'gp', and 'gene_table', respectively.
#' 
#' @param id (Required)
#' List of UIDs provided either as a character vector, as an
#' \code{\linkS4class{esearch}} instance, or by reference to a web environment
#' and a query key obtained directly from previous calls to
#' \code{\linkS4class{esearch}} (if \code{usehistory} was set \code{TRUE}),
#' \code{\linkS4class{epost}} or \code{\linkS4class{elink}}.
#' If UIDs are provided as a plain character vector, \code{db} must be
#' specified explicitly, and all of the UIDs must be from the database
#' specified by \code{db}.
#' @param db (Required only when \code{id} is a character vector of UIDs)
#' Database from which to retrieve records. See
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/table/chapter2.chapter2_table1/?report=objectonly}{here}
#' for the supported databases.
#' @param query_key An integer specifying which of the UID lists attached
#' to a user's Web Environment will be used as input to \code{efetch}.
#' (Usually obtained drectely from objects returned by previous
#' \code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
#' \code{\linkS4class{elink}} calls.)
#' @param WebEnv A character string specifying the Web Environment that
#' contains the UID list. (Usually obtained directely from objects returned
#' by previous \code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
#' \code{\linkS4class{elink}} calls.)
#' @param rettype A character string specifying the report type returned,
#' such as 'abstract' or 'medline' from PubMed, 'gp' or 'fasta' from
#' protein, or 'gb', 'gbwithparts, or 'fasta_cds_na' from nuccore.
#' See
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
#' for allowed values for each database.
#' @param retmode A character string specifying the data mode of the
#' records returned, such as plain text, XML, or asn.1. See 
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
#' for allowed values for each database.
#' @param retstart Numeric index of the first record to be retrieved.
#' @param retmax Total number of records from the input set to be retrieved.
#' @param strand Strand of DNA to retrieve. (1: plus strand, 2: minus strand)
#' @param seq_start First sequence base to retrieve.
#' @param seq_stop Last sequence base to retrieve.
#' @param complexity Data content to return. (0: entire data structure,
#' 1: bioseq, 2: minimal bioseq-set, 3: minimal nuc-prot, 4: minimal pub-set)
#' @return An \code{efetch} instance.
#' @seealso \code{\link{efetch.batch}} for downloading more than about 500
#' data records.
#' \code{\link{content}} to retrieve data from \code{\linkS4class{efetch}}
#' objects.
#' @example inst/examples/efetch.r
#' @export
#' @autoImports
efetch <- function (id, db = NULL, query_key = NULL, WebEnv = NULL,
                    rettype = NULL, retmode = NULL, retstart = NULL,
                    retmax = 500, strand = NULL, seq_start = NULL,
                    seq_stop = NULL, complexity = NULL) {
  
  ## id may be missing if WebEnv and query_key are provided
  if ((is.null(query_key) || is.null(WebEnv)) && missing(id)) {
    stop("No UIDs provided")
  }
  
  ## if WebEnv and query_key are provided, db must also be provided
  if (not.null(query_key) && not.null(WebEnv) && is.null(db)) {
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
  
  new("efetch", url = o@url, content = o@content, error = list(),
      database = db, retmode = retmode, rettype = rettype)
}

#' Retrieve batches of data records in the requested format from NCBI
#'
#' \code{efetch.batch} retrieves large data sets from NCBI in batches.
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}{EUtilities}
#' for additional information.
#' 
#' @param id (Required)
#' List of UIDs provided (via the Entrez History server) by an
#' \code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
#' \code{\linkS4class{elink}} object.
#' @param chunk_size Number of records downloaded as a batch (default: 200;
#' maximum: 500).
#' @param rettype A character string specifying the record view returned,
#' such as 'abstract' or 'medline' from PubMed, or 'gp' or 'fasta' from
#' protein.
#' See
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
#' for allowed values for each database.
#' @param retmode A character string specifying the data format of the
#' records returned, such as plain text, XML, or asn.1.
#' See
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
#' for allowed values for each database.
#' @param strand Strand of DNA to retrieve. (1: plus strand, 2: minus strand)
#' @param seq_start First sequence base to retrieve.
#' @param seq_stop Last sequence base to retrieve.
#' @param complexity Data content to return. (0: entire data structure,
#' 1: bioseq, 2: minimal bioseq-set, 3: minimal nuc-prot, 4: minimal pub-set)
#' @return An \code{\linkS4class{efetch}} object.
#' @export
#' @example inst/examples/efetch.batch.r
#' @autoImports
efetch.batch <- function (id, chunk_size=200, rettype=NULL, retmode=NULL,
                          strand=NULL, seq_start=NULL, seq_stop=NULL,
                          complexity=NULL) {
  
  if (class(id) %ni% c("esearch", "epost", "elink"))
    stop("efetch.batch() expects an 'esearch', 'epost', or 'elink' object")
  
  max_chunk <- 500
  if (chunk_size > max_chunk) {
    warning(sprintf("The maximum downloadable chunk size is %s."), 
            max_chunk, call.=FALSE)
    chunk_size <- max_chunk
  }
  
  if (count(id) <= max_chunk) {
    res <- efetch(id=id, rettype=rettype, retmode=retmode, retstart=NULL,
                  retmax=NULL, strand=strand, seq_start=seq_start,
                  seq_stop=seq_stop, complexity=complexity)
  } else {
    n_chunks <- count(id)%/%chunk_size
    retstart <- seq(from=1, to=n_chunks*chunk_size, by=chunk_size)
    res <- new("efetch")
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
