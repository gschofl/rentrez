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
##' @aliases write,efetch-method
##' @aliases c,efetch-method
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

##' @export
setMethod("write",
          signature(x = "efetch"),
          function (x, file = "data", append = FALSE, sep = "") {
            write(x = x@data, file = file, append = append, sep = sep)
          })

##' @export
setMethod("c",
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
            
            new("efetch", url=url, data=data, database=db,
                mode=mode, type=type)
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
##' @param rettype A character string specifying the record view returned,
##' such as 'abstract' or 'medline' from PubMed, or 'gp' or 'fasta' from
##' protein.
##' See \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
##' for allowed values for each database.
##' @param retmode A character string specifying the data format of the
##' records returned, such as plain text, XML, or asn.1. See 
##' \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
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
  
  ## get id, or WebEnv and query_key #######################################
  # if no Web Environment is provided extract WebEnv and query_key from id 
  # (or take the idList if an esearch object with usehistory=FALSE was
  # provided)
  if (is.null(query_key) && is.null(WebEnv)) {
    env_list <- .getId(id)
    WebEnv <- env_list$WebEnv
    query_key <- env_list$query_key
    id <- .collapseUIDs(env_list$id)
  } else
    id <- NULL
  
  ## restrict retmax
  if (is.null(retmax) || retmax > 200) retmax <- 200
  
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
  
  o <- .query("efetch", db=db, id=id, query_key=query_key,
              WebEnv=WebEnv, retmode=retmode, rettype=rettype,
              retstart=retstart, retmax=retmax, strand=strand,
              seq_start=seq_start, seq_stop=seq_stop,
              complexity=complexity)

  new("efetch", url=o@url, data=o@data, database=db,
      mode=retmode, type=rettype)
}

##' Retrieve batches of data records in the requested format from NCBI
##'
##' \code{efetch.batch} retrieves large data sets from NCBI in batches.
##' 
##' See the online documentation at
##' \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}
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
##' See \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
##' for allowed values for each database.
##' @param retmode A character string specifying the data format of the
##' records returned, such as plain text, XML, or asn.1. See 
##' \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
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
##' @examples
##'   query <- 'chimpanzee[orgn] and biomol mrna[prop]'
##'   (s <- esearch(query, "nuccore", usehistory=TRUE))
##'   ## This takes some time
##'   f <- efetch.batch(s, chunk_size=200, rettype="fasta")
##'   write(f, file="~/chimp_mrna.fa")
efetch.batch <- function (id,
                          chunk_size=200,
                          rettype=NULL,
                          retmode=NULL,
                          strand=NULL,
                          seq_start=NULL,
                          seq_stop=NULL,
                          complexity=NULL) {
  
  if (!is(id, "esearch") && !is(id, "epost") && !is(id, "elink"))
    stop("efetch.batch expects an 'esearch', 'epost', or 'elink' object")
  
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


# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

