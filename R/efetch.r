### Efetch #################################################################
##' @include utils.r
##' @include blast-classes.r
##' @include eutil-classes.r
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
##' @aliases write,efetch-method
##' @aliases c,efetch-method
##' @aliases getSeq,efetch-method
.efetch <-
  #### efetch-class ####
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
          #### show-method ####
          signature(object = "efetch"),
          function (object) {
            cat(sprintf("EFetch query using the %s database.\nQuery url: %s\n\n",
                        sQuote(object@database), sQuote(object@url)))
            cat(object@data)
            return(invisible(NULL))
          })

##' @export
setMethod("write",
          #### write-method ####
          signature(x = "efetch"),
          function (x, file = "data", append = FALSE, sep = "") {
            write(x = x@data, file = file, append = append, sep = sep)
          })

##' @export
setMethod("c",
          #### c-method ####
          signature(x = "efetch"),
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
            data <- c(x@data, unlist(lapply(list(...), slot, "data")))
            
            .efetch(url=url, data=data, database=db,
                    mode=mode, type=type)
          })


##' Parse \code{\link{efetch}} retrived records into R data structures 
##' 
##' @usage parse(x, seqtype=c('DNA','RNA','AA'),
##'   outfmt=c('Biostring', 'DNAbin', 'String'))
##' 
##' @param x an \code{\link{efetch-class}} object.
##' @param seqtype sequence type. One of 'DNA' (default), 'RNA', or 'AA'.
##' @param outfmt Output format. One of 'Biostring' (default),
##' \code{\link[ape]{DNAbin}}, or a character vector ('String').
##' 
##' @return An object specified by \code{outfmt} is created.
##' 
##' @export
##' @docType methods
##' @rdname efetch-methods
setGeneric("parse",
           #### parse-generic ####
           function(x, ...) {
             standardGeneric("parse")
           })

##' @export
setMethod("parse",
          #### parse-method ####
          signature(x="efetch"),
          function(x, ...) {
            if (grepl("^fasta", x@type) && x@mode == "text") {
              return(.getFasta(x=x, seqtype=c("DNA","RNA","AA"), 
                               outfmt=c("Biostring", "DNAbin", "String")))
            } else if (grepl("^gb|^gp", x@type) && x@mode == "text") {
              dbs <- biofiles::readGB(x, with_sequence=TRUE, force=FALSE)
              records <- list()
              for (db in dbs) {
                records <- c(records, list(biofiles::initGB(db)))
              }
              names(records) <- vapply(records, "[[", "accession", FUN.VALUE=character(1))
              return(records)
            } else {
              stop("Only fasta and GenBank flat files are supported at the moment")
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
##' Database from which to retrieve records.
##' @param query_key An integer specifying which of the UID lists attached
##' to a user's Web Environment will be used as input to \code{efetch}.
##' (Usually obtained drectely from objects returned by previous
##' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}} calls.)
##' @param WebEnv A character string specifying the Web Environment that
##' contains the UID list. (Usually obtained directely from objects returned
##' by previous \code{\link{esearch}}, \code{\link{epost}} or
##' \code{\link{elink}} calls.)
##' @param rettype A character string specifying the report type returned,
##' such as 'abstract' or 'medline' from PubMed, or 'gp' or 'fasta' from
##' protein.
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
##' @return An \code{\link{efetch-class}} object.
##' @seealso \code{\link{efetch.batch}} for downloading more than about 500
##' data records.
##'
##' @export
##' @example inst/examples/efetch.r
efetch <- function (id,
                    db=NULL,
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
  
  ## get db ################################################################
  # if no db name is provided extract the database name directly from
  # id if it's an esearch, epost or elink object
  if (is.null(db) && is.null(db <- .getDb(id)))
    stop("No database name provided")
  
  ## set default rettype and retmode for some databases
  if (is.null(rettype)) {
    rettype <- switch(db,
                      pubmed="medline",
                      nucleotide="gbwithparts",
                      nuccore="gbwithparts",
                      protein="gp",
                      gene="gene_table")
  }
  
  if (is.null(retmode)) {
    retmode <- switch(db,
                      pubmed="text",
                      nucleotide="text",
                      nuccore="text",
                      protein="text",
                      gene="text")
  }
  
  ## get id, or WebEnv and query_key #######################################
  if (is.null(query_key) && is.null(WebEnv)) {
    # extract WebEnv and query_key from id or take the idList/linkList if an
    # esearch/elink object with usehistory=FALSE was provided.
    env_list <- .getId(id)
    WebEnv <- env_list$WebEnv
    query_key <- env_list$query_key
    record_count <- env_list$count # number of  items to be downloaded
    count <- length(env_list$id) # number of UIDs to be uploaded
    id <- env_list$id
    
    if (!is.na(record_count) &&
        record_count > 500 && (is.null(retmax) || retmax > 500)) {
      # if record_count exceeds 500 issue a warning and recommend
      # efetch.batch()
      message(gettextf("You are attempting to download %s records.\nOnly the first 500 were downloaded. Use efetch.batch() instead.",
              max(c(record_count, retmax))))
      retmax <- 500
      id <- id[seq_len(500)]
    }
    else if (is.na(record_count)) {
      # this takes care of the case where we use elink with
      # cmd='neighbor_history' and don't actually know how many UIDs are
      # stored on the history server
      retmax <- 500
    }
  } else {
    # if there is an explicit user provided WebEnv we simply set id=NULL
    # count=0, and restrict retmax to 500
    count <- 0
    id <- NULL
    retmax <- 500
  }
  
  if (count > 100) {
    # use HTTP POST if uploading more than 100 user provided UIDs.
    o <- .httpPOST(eutil="efetch", db=db, id=.collapse(id),
                   WebEnv=WebEnv, retmode=retmode, rettype=rettype,
                   retstart=as.character(retstart), retmax=as.character(retmax),
                   strand=as.character(strand),
                   seq_start=as.character(seq_start),
                   seq_stop=as.character(seq_stop),
                   complexity=as.character(complexity))
  }
  else {
    o <- .query("efetch", db=db, id=.collapse(id), query_key=query_key,
                WebEnv=WebEnv, retmode=retmode, rettype=rettype,
                retstart=retstart, retmax=retmax, strand=strand,
                seq_start=seq_start, seq_stop=seq_stop,
                complexity=complexity)
  }

  .efetch(url=o@url, data=o@data, database=db, mode=retmode, type=rettype)
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
##' \code{\link{esearch-class}}, \code{\link{epost-class}} or
##' \code{\link{elink-class}} object.
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
##' @return An \code{\link{efetch-class}} object.
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

#' Extract fasta from efetch
#' 
#' @importFrom Biostrings read.DNAStringSet
#' @importFrom Biostrings read.RNAStringSet
#' @importFrom Biostrings read.AAStringSet
#' @importFrom ape read.dna
.getFasta <- function (x,
                       seqtype=c("DNA","RNA","AA"),
                       outfmt=c("Biostring", "DNAbin", "String"))
{
  seqtype <- match.arg(seqtype)
  format <- match.arg(outfmt)

  if (!grepl(pattern="^>", x@data))
    stop("Does not appear to contain a valid fasta file")
  
  if (format == "Biostring") {
    # stopifnot(require("Biostrings"))
    f_tmp <- tempfile(fileext=".fa")
    write(x, file=f_tmp)
    fasta <- switch(seqtype,
                    DNA=read.DNAStringSet(f_tmp, use.names=TRUE),
                    RNA=read.RNAStringSet(f_tmp, use.names=TRUE),
                    AA=read.AAStringSet(f_tmp, use.names=TRUE)) 
    unlink(f_tmp)
    return(fasta)
  }
  else if (format == "DNAbin") {
    # stopifnot(require("ape"))
    fasta <- ape::read.dna(file=textConnection(x@data), format="fasta")
    return(fasta)  
  }
  else if (format == "String") {
    fasta_split <- strsplit(x@data, "\n")[[1]]
    desc_idx <- which(grepl(pattern="^>", fasta_split))
    desc <- sub(">", "", fasta_split[desc_idx])
    fasta <- paste0(fasta_split[-desc_idx], collapse="")
    names(fasta) <- desc
    return(fasta)
  }
}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

