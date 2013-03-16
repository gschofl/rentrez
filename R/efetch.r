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
#' @slot content A character vector holding the unparsed
#' contents of a request to Entrez.
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
                        rettype = "character",
                        retmode = "character"),
         prototype(database = NA_character_,
                   rettype = NA_character_,
                   retmode = NA_character_),
         contains = "eutil")


# accessor-methods -------------------------------------------------------


setMethod("database", "efetch", function(x) x@database)

setMethod("retmode", "efetch", function(x) x@retmode)

setMethod("rettype", "efetch", function(x) x@rettype)


#' @autoImports
setMethod("content", "efetch",
          function (x, as = NULL) {
            as <- as %|null|% retmode(x)
            if (as == "asn.1") 
              as <- "text"
            as <- match.arg(as, c("text", "xml"))
            callNextMethod(x = x, as = as)
          })


# show-method ------------------------------------------------------------


setMethod("show", "efetch",
          function (object) {
            
            if (retmode(object) == "xml")
              print(content(object))
            else
              cat(content(object))
            
            cat(sprintf("EFetch query using the %s database.\nQuery url: %s\n",
                        sQuote(database(object)), sQuote(queryUrl(object))))
            cat(sprintf("Retrieval type: %s, retrieval mode: %s\n",
                        sQuote(rettype(object)), sQuote(retmode(object))))
            
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
            write(x = content(x, "text"), file = file, append = append)
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
            
            url <- c(queryUrl(x), vapply(list(...), queryUrl, character(1)))
            content <- c(content(x, "text"),
                         unlist(lapply(list(...), content, as="text")))
            
            new("efetch", url = url, content = content, error = list(),
                database = db, retmode = rm, rettype = rt)
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
#' \code{\linkS4class{esearch}} (if \code{usehistory = TRUE}),
#' \code{\linkS4class{epost}} or \code{\linkS4class{elink}}.
#' If UIDs are provided as a plain character vector, \code{db} must be
#' specified explicitly, and all of the UIDs must be from the database
#' specified by \code{db}.
#' @param db (Required only when \code{id} is a vector of UIDs)
#' Database from which to retrieve records. See
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/table/chapter2.chapter2_table1/?report=objectonly}{here}
#' for the supported databases.
#' @param rettype A character string specifying the retrieval type,
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
#' @param query_key An integer specifying which of the UID lists attached
#' to a user's Web Environment will be used as input to \code{efetch}.
#' (Usually obtained drectely from objects returned by previous
#' \code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
#' \code{\linkS4class{elink}} calls.)
#' @param WebEnv A character string specifying the Web Environment that
#' contains the UID list. (Usually obtained directely from objects returned
#' by previous \code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
#' \code{\linkS4class{elink}} calls.)
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
efetch <- function (id, db = NULL, rettype = NULL, retmode = NULL,
                    retstart = NULL, retmax = NULL, query_key = NULL,
                    WebEnv = NULL, strand = NULL, seq_start = NULL,
                    seq_stop = NULL, complexity = NULL) {
  
  ## extract query parameters
  params <- get_params(id, db, WebEnv, query_key)
  
  # set default rettype and retmode for a given db
  r <- set_record_type(params$db, rettype, retmode)
  
  if (is.null(retmax))
    retmax <- Inf
  
  if (retmax > 500 && (is.finite(params$count) && (params$count > 500))) {
    # if record_count exceeds 500 issue a warning and recommend
    # efetch.batch()
    message(gettextf("You are attempting to download %s records.\nOnly the first 500 are downloaded. Use efetch.batch() instead.",
                     min(c(params$count, retmax))))
    retmax <- 500
    params$uid <- params$uid[seq_len(500)]
  } else if (is.na(params$count)) {
    # this takes care of the cases where we don't actually know how many UIDs
    # are stored on the history server
    # message("A single download request is restricted to 500 records.\nUse efetch.batch() to download more records.")
    retmax <- 500
  }

  method <- if (length(params$uid) < 100) "GET" else "POST"
  o <- .equery('efetch', method, db = params$db, id = .collapse(params$uid),
               query_key = params$query_key, WebEnv = params$WebEnv,
               retmode = r$retmode, rettype = r$rettype, retstart = retstart,
               retmax = retmax, strand = strand, seq_start = seq_start,
               seq_stop = seq_stop, complexity = complexity)
  
  error <- error(o)
  error <- if (r$retmode == "xml" && all_empty(error)) checkErrors(o, FALSE) else error
  new("efetch", url = queryUrl(o), content = content(o, "text"),
      error = error, database = params$db,
      retmode = r$retmode %||% NA_character_,
      rettype = r$rettype %||% NA_character_)
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
#' @param retmax Total number of records from the input set to be retrieved.
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
                          retmax=NULL, strand=NULL, seq_start=NULL,
                          seq_stop=NULL, complexity=NULL) {
  
  if (class(id) %ni% c("esearch", "epost", "elink"))
    stop("efetch.batch() expects an 'esearch', 'epost', or 'elink' object")
  
  max_chunk <- 500
  if (chunk_size > max_chunk) {
    warning(sprintf("The maximum downloadable chunk size is %s."), 
            max_chunk, call.=FALSE)
    chunk_size <- max_chunk
  }
  
  count <- count(id)
  if (not.null(retmax) && retmax < count) {
    count <- retmax
  }
  
  if (count <= max_chunk) {
    res <- efetch(id=id, rettype=rettype, retmode=retmode, retstart=NULL,
                  retmax=NULL, strand=strand, seq_start=seq_start,
                  seq_stop=seq_stop, complexity=complexity)
  } else {
    n_chunks <- count%/%chunk_size
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
