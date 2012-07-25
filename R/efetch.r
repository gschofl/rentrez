
# efetch-class -----------------------------------------------------------

##' @include utils.r
##' @include eutil.r
##' @include blast-classes.r
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


##' @export
setMethod("show", "efetch",
          function (object) {
            cat(sprintf("EFetch query using the %s database.\nQuery url: %s\n\n",
                        sQuote(object@database), sQuote(object@url)))
            cat(object@content)
            return(invisible(NULL))
          })


# write-method -----------------------------------------------------------


##' @export
setMethod("write", "efetch",
          function (x, file = "data", append = FALSE, sep = "") {
            write(x = x@content, file = file, append = append, sep = sep)
          })


# c-method ---------------------------------------------------------------


##' @export
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
            
            .efetch(url=url, content=content, database=db,
                    mode=mode, type=type)
          })


# content-method ---------------------------------------------------------


##' @export
setMethod("content", "efetch",
          function(x, parse = TRUE, format = c("Biostrings", "DNAbin", "String"), ...) {
            if (isTRUE(parse)) {
              if (x@database == "pubmed") {
                return( .parsePubmed(x) )
              }
              if (x@database %in% c("protein","nucleotide","nuccore")) {
                return( .parseSequence(x, format = format) )
              }
              return( x@content )

            } else {
              x@content
            }
          })


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
  ref$abstract
}


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
                      nucleotide="gb",
                      nuccore="gb",
                      protein="gp",
                      gene="gene_table")
  }
  
  if (is.null(retmode)) {
    retmode <- switch(db,
                      pubmed="xml",
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
    } else if (is.na(record_count)) {
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
    o <- .httpPOST("efetch", db=db, id=.collapse(id),
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

  .efetch(url=o@url, content=o@content, database=db, mode=retmode, type=rettype)
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

#' Extract fasta from efetch
#' 
#' @importFrom Biostrings read.DNAStringSet
#' @importFrom Biostrings read.AAStringSet
#' @importFrom ape read.dna
#' @importFrom phangorn read.aa
.parseSequence <- function (x, format = c("Biostrings", "DNAbin", "String")) {
  
  ## if fasta return a DNAStringSet or AAStringSet
  if (grepl("^fasta", x@type) && x@mode == "text") {
    
    format <- match.arg(format)
    
    if (!grepl("^>", x@content)) {
      warning("Does not appear to contain a valid fasta file")
      return( x@content )
    }
    
    if (x@database %in% c("nucleotide","nuccore")) {
      seqtype <- "DNA"
    } else if (x@database == "protein") {
      seqtype <- "AA"
    }
    
    if (format == "Biostrings") {
      f_tmp <- tempfile(fileext=".fa")
      write(x, file=f_tmp)
      fasta <- switch(seqtype,
                      DNA=read.DNAStringSet(f_tmp, use.names=TRUE),
                      AA=read.AAStringSet(f_tmp, use.names=TRUE)) 
      unlink(f_tmp)
      return( fasta )
    }
    
    if (format == "DNAbin") {
      fasta <- switch(seqtype,
                      DNA=ape::read.dna(file=textConnection(x@content), format="fasta"),
                      AA=phangorn::read.aa(file=textConnection(x@content), format="fasta"))
      return( fasta )  
    }
    
    if (format == "String") {
      fasta_split <- strsplit(x@content, "\n")[[1]]
      desc_idx <- which(grepl(pattern="^>", fasta_split))
      desc <- sub(">", "", fasta_split[desc_idx])
      fasta <- paste0(fasta_split[-desc_idx], collapse="")
      names(fasta) <- desc
      return( fasta )
    }
  }
  
  ## if gp or gb return gbRecord
  if (grepl("^gb|^gp", x@type) && x@mode == "text") {
    dbs <- biofiles::readGB(x, with_sequence=TRUE, force=FALSE)
    
    if (length(dbs) == 1) {
      return(biofiles::initGB(dbs))
    } else {
      records <- list()
      for (db in dbs) {
        records <- c(records, list(biofiles::initGB(db)))
      }
      names(records) <- vapply(records, "[[", "accession", FUN.VALUE=character(1))
      return( records )
    }
  }
  
  ## if xml return parsed xml tree
  if (x@mode == "xml") {
    return( xmlParse(x@content) )
  }
  
  ## otherwise return the raw content
  return( x@content )
}

.parsePubmed <- function (x) {
  
  if (x@mode != 'xml') {
    return( x@content )
  }
  
  doc <- getNodeSet(xmlRoot(xmlParse(x@content)), '//PubmedArticle')
  
  reff <- lapply(doc, function (art) {
    
    #     art <- xmlDoc(doc[[1]])
    art <- xmlDoc(art)
    
    author <- {
      lastName <- xpathApply(art, "//AuthorList//LastName", xmlValue)
      foreName <- xpathApply(art, "//AuthorList//ForeName", xmlValue)
      list(author = do.call(personList,
                            Map(person, given=foreName, family=lastName)))
    }
    
    issue <- list(
      volume = xpathSApply(art, '//JournalIssue/Volume', xmlValue),
      number = xpathSApply(art, '//JournalIssue/Issue', xmlValue),
      year = {
        year <- xpathSApply(art, '//JournalIssue/PubDate/Year', xmlValue)
        medlineDate <- xpathSApply(art, '//JournalIssue/PubDate/MedlineDate', xmlValue)
        if (length(year) > 0) year else medlineDate
      },
      month = xpathSApply(art, '//JournalIssue/PubDate/Month', xmlValue),
      pages = xpathSApply(art, '//Pagination/MedlinePgn', xmlValue)
    )
    
    journal <- list(
      issn = xpathSApply(art, '//Journal/ISSN', xmlValue),
      journal = xpathSApply(art, '//Journal/Title', xmlValue),
      abbrev = xpathSApply(art, '//Journal/ISOAbbreviation', xmlValue) 
    )
    
    article <- list(
      title = xpathSApply(art, '//ArticleTitle', xmlValue),
      abstract = {
        abs <- xpathSApply(art, '//Abstract/AbstractText', xmlValue)
        headers <- xpathSApply(art, '//Abstract/AbstractText', xmlGetAttr, "Label")
        if (is.null(headers[[1]])) {
          abs
        } else {
          paste0(headers, ": ", abs, collapse="\n")
        }
      },
      doi = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="doi"]', xmlValue),
      pii = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pii"]', xmlValue),
      pmid = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pubmed"]', xmlValue),
      pmc = xpathSApply(art, '//ArticleIdList/ArticleId[@IdType="pmc"]', xmlValue)
    )
    
    affiliation <- list(
      affiliation = xpathSApply(art, "//Affiliation", xmlValue)
    )
    
    issue[vapply(issue, function (x) length(x) < 1L, logical(1))] <- ""
    journal[vapply(journal, function (x) length(x) < 1L, logical(1))] <- ""
    article[vapply(article, function (x) length(x) < 1L, logical(1))] <- ""
    affiliation[vapply(affiliation, function (x) length(x) < 1L, logical(1))] <- ""
    
    free(art)
    
    ref <- bibentry('Article', other=c(author, article, journal, issue, affiliation))
    ref
    
  })
  
  reff <- do.call("c", reff)
  reff
  
}



# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

