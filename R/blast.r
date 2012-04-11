### Blast ##################################################################
##' @include eutil.r
##' @include utils.r
NULL

##' blast class
##' 
##' blast is an S4 class that provides a container for data retrived by 
##' calls to the NCBI Blast utility.
##' 
##' blast objects have nine slots:
##' \describe{
##'   \item{bitscore}{}
##'   \item{evalue}{}
##'   \item{mlog.evalue}{}
##'   \item{gi}{}
##'   \item{accession}{}
##'   \item{query}{}
##'   \item{params}{}
##'   \item{hit_table}{}
##'   \item{hsp_list}{}
##' }
##' 
##' @name blast-class
##' @rdname blast-class
##' @exportClass blast
##' @aliases show,blast-method
##' @aliases blast,blast-method
.blast <- setClass("blast",
                   representation(bitscore = "numeric",
                                  evalue = "numeric",
                                  mlog.evalue = "numeric",
                                  gi = "character",
                                  accession = "character",
                                  query = "character",
                                  params = "character",
                                  hit_table = "data.frame",
                                  hsp_list = "list"),
                   prototype(bitscore = NA_real_,
                             evalue = NA_real_,
                             mlog.evalue = NA_real_,
                             gi = NA_character_,
                             accession = NA_character_,
                             query = NA_character_,
                             params = NA_character_,
                             hit_table = data.frame(),
                             hsp_list = list()))

##' @export
setMethod("show",
          signature(object = "blast"),
          function (object) {
            cat(sprintf("Blast search using %s on the %s database.\nQuery name: %s\n\n",
                        sQuote(object@params["program"]),
                        sQuote(object@params["db"]),
                        object@query))
            print(format(data.frame(
                definition = substr(tbl@hit_table$sdef, 0, getOption("width")/3),
                gi = object@gi,
                evalue=object@evalue,
                mlog.evalue=object@mlog.evalue,
                bitscore=object@bitscore),
              digits=2))
            return(invisible(NULL))
          })

##' Do a BLAST search using the qblast API
##' 
##' Run blast against the NCBI sequence databases using the qblast URLAPI.
##' 
##' @param query  An accession number, a gi, or a fasta sequence provided
##' either as an \code{\link[Biostrings]{XString}} or
##' \code{\link[Biostrings]{XStringSet}} object, or as a named character
##' vector.
##' @param program Specifies the blast program to use. One of 'megablast'
##' (default), 'blastn', 'blastp', 'rpsblast', blastx', 'tblastn', 'tblastx'.
##' @param database Which database to search against. E.g. "nr".
##' @param format Output format. 'view' shows the blast results in a browser
##' and returns the html response from the NCBI web server; 'table' returns
##' a \code{\link{blast-class}} object.
##' @param entrez_query An Entrez query.
##' @param n_hits The maximum number of hits to return.
##' @param .params Additional parameters of the qblast API.
##' @param display Display the result in a web  browser
##' @param update_time How often to poll the blast server for results
##' 
##' @return A \code{\link{blast-class}} object.
##'
##' @importFrom stringr str_match
##' @importFrom RCurl getForm
##' @importFrom Biostrings toString
##'
##' @export
##' @examples
##'  ##
qblast <- function(query,
                   program=c("megablast","blastn","blastp",
                             "rpsblast","blastx","tblastn",
                             "tblastx"),
                   database="nr",
                   format=c("view","table"),
                   entrez_query="",
                   n_hits=20,
                   .params=list(expect=1e-3, filter="L"),
                   display=FALSE,
                   update_time=4)
{
  qbd <- megablast <- rpsblast <- FALSE
  program <- match.arg(program)
  format <- match.arg(format)
  
  if (nchar(entrez_query) > 0L) {
    .params <- c(.params, entrez_query=.escape(entrez_query))
  }
  
  if (format == "view") {
    format <- "HTML"
    alignment_view <- "Pairwise"
    display <- TRUE
  }
  else if (format == "table") {
    format <- "XML"
    alignment_view <- "Pairwise"
  }
  
  if (program == "megablast") {
    program <- "blastn"
    megablast <- TRUE
  }
  
  if (program == "rpsblast") {
    program <- "blastp"
    rpsblast <- TRUE
  }
  
  if (is(query, "XStringSet") || is(query, "BString")) {
    query <- paste0(">", names(query)[[1L]], "\n", Biostrings::toString(query[[1L]]))
    # query <- .escape(query)
    qbd <- TRUE
  } else if (!is.null(names(query))) {
    query <- paste0(">", names(query)[[1L]], "\n", as.character(query[[1L]]))
    # query <- .escape(query)
    qbd <- TRUE
  } else {
    query <- as.character(query)
  }
  
  # .params=list(expect=1e-3, filter="L")
  .params <- c(QUERY=query,
               QUERY_BELIEVE_DEFLINE=if (qbd) "true" else "false",
               PROGRAM=program,
               DATABASE=database,
               HITLIST_SIZE=n_hits,
               MEGABLAST=if (megablast) "yes" else "no",
               SERVICE=if (rpsblast) "rpsblast" else "plain",
               CLIENT="web",
               CMD="Put",
               .params)
  
  names(.params) <- toupper(names(.params)) 
  base_url <- "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi"   
  response <- getForm(base_url, .params=.params, style="post")
  # displayHTML(response, unlink=F)
  # parse out the request id and the estimated time to completion
  xpath <- "/descendant::comment()[contains(.,'RID =')]"
  r <- xpathSApply(htmlParse(response), xpath, xmlValue)
  rid <- str_match(r, "RID = (.[^(\n)]*)")[,2]
  
  # poll for results
  while (TRUE) {
    
    Sys.sleep(update_time)
    cat(sprintf(" Searching ... (update in %s seconds)\n", update_time))
    response <- getForm(uri=base_url,
                        RID=rid,
                        FORMAT_OBJECT="SearchInfo",
                        CMD="Get",
                        style="post")
    
    xpath <- "/descendant::comment()[contains(.,'Status=')]"
    status <- xpathSApply(htmlParse(response), xpath, xmlValue)
    status <- str_match(status, "Status=(.[^(\n)]*)")[,2]
    cat(sprintf("Status: %s\n", status))
    
    if (status == "WAITING") {
      next
    }
    
    if (status == "FAILED") {
      stop(sprintf("Search %s failed\n", rid))
    }
    
    if (status == "UNKNOWN") {
      stop(sprintf("Search %s expired\n", rid))
    }
    
    if (status == "READY") {
      xpath <- "/descendant::comment()[contains(.,'ThereAreHits=')]"
      there_are_hits <- xpathSApply(htmlParse(response), xpath, xmlValue)
      there_are_hits <- str_match(there_are_hits, "ThereAreHits=(.[^(\n)]*)")[,2]
      if (there_are_hits == "yes") {
        break
      }
      else {
        cat("No hits found\n")
      }
    }
  }
  
  # retrieve results
  response <- getForm(uri=base_url,
                      RID=rid,
                      FORMAT_TYPE=format,
                      FORMAT_OBJECT="Alignment",
                      DESCRIPTIONS=n_hits,
                      ALIGNMENTS=n_hits,
                      ALIGNMENT_VIEW=alignment_view,
                      NEW_VIEW="true",
                      DISPLAY_SORT="2",
                      NCBI_GI="true",
                      CMD="Get",
                      style="post")
  
  if (format == "HTML") {
    displayHTML(response, unlink=TRUE)
    return(invisible(response))
  }
  
  #if (format == "Text") {
  #  if (display) displayHTML(response)
  #  response <- xpathApply(htmlParse(response), "//pre", xmlValue)[[1L]]
  #  return(readBLAST(textConnection(strsplit(response, "\n")[[1L]])))
  #}
  
  if (format == "XML") {
    if (display) displayHTML(response, unlink=FALSE)
    return(parseBlastXml(response))
  }
}

##' Parse xml blast output
##' 
##' @param blast_output Blast output in XML format
##' 
##' @importFrom Biostrings BStringSet
##' @importFrom stringr str_split
##' @importFrom stringr str_split_fixed
##' @importFrom stringr str_replace_all
##' 
##' @keywords internal
parseBlastXml <- function (blast_output)
{
  
  gapsOpen <- function (s1, s2) {
    length(gregexpr("-+", s1)[[1L]]) + length(gregexpr("-+", s2)[[1L]])
  }
  
  #stopifnot(require(XML))
  doc <- xmlRoot(xmlParse(blast_output))
  ## statistics
  program <- xpathSApply(doc, "//BlastOutput_program", xmlValue)
  version <- xpathSApply(doc, "//BlastOutput_version", xmlValue)
  db <- xpathSApply(doc, "//BlastOutput_db", xmlValue)
  params <- gsub(";\\b", "", xpathApply(doc, "//Parameters/*", xmlValue))
  names(params) <-
    gsub("-", ".", 
      sapply(xpathApply(doc, "//Parameters/*", xmlName), function (x) {
        strsplit(x, "_")[[1L]][2]
      }))
  params <- c(program=program, version=version, db=db, params)
  ## query
  iter_num <- xpathApply(doc, "//Iteration_iter-num", xmlValue)[[1L]]
  query_id <- xpathApply(doc, "//Iteration_query-ID", xmlValue)[[1L]]
  query_def <- xpathApply(doc, "//Iteration_query-def", xmlValue)[[1L]]
  query_len <- xpathApply(doc, "//Iteration_query-len", xmlValue)[[1L]]
  
  ## hit.table
  hits <- getNodeSet(doc, "//Hit")
  hit_table <- list()
  hsp_list <- list()
  # i <- 3
  for (i in seq_along(hits)) {
    hit <- xmlDoc(hits[[i]])
    id <- xpathSApply(hit, "//Hit_id", xmlValue)
    def <- xpathSApply(hit, "//Hit_def", xmlValue)
    def <- paste(id, str_replace_all(def, " >", ";")[[1L]])
    gi <- str_split_fixed(id, "\\|", 3)[,2]
    accn <- xpathSApply(hit, "//Hit_accession", xmlValue)
    len <- xpathSApply(hit, "//Hit_len", xmlValue)
    
    ## HSPs
    num <- as.numeric(xpathSApply(hit, "//Hsp_num", xmlValue))
    bit_score <- as.numeric(xpathSApply(hit, "//Hsp_bit-score", xmlValue))
    score <- as.numeric(xpathSApply(hit, "//Hsp_score", xmlValue))
    evalue <- as.numeric(xpathSApply(hit, "//Hsp_evalue", xmlValue))
    query_from <- as.numeric(xpathSApply(hit, "//Hsp_query-from", xmlValue))
    query_to <- as.numeric(xpathSApply(hit, "//Hsp_query-to", xmlValue))
    hit_from <- as.numeric(xpathSApply(hit, "//Hsp_hit-from", xmlValue))
    hit_to <- as.numeric(xpathSApply(hit, "//Hsp_hit-to", xmlValue))
    query_frame <- as.numeric(xpathSApply(hit, "//Hsp_query-frame", xmlValue))
    hit_frame <- as.numeric(xpathSApply(hit, "//Hsp_hit-frame", xmlValue))
    identity <- as.numeric(xpathSApply(hit, "//Hsp_identity", xmlValue))
    positive <- as.numeric(xpathSApply(hit, "//Hsp_positive", xmlValue))
    gaps <- as.numeric(xpathSApply(hit, "//Hsp_gaps", xmlValue))
    align_len <- as.numeric(xpathSApply(hit, "//Hsp_align-len", xmlValue))
    qseq <- BStringSet(xpathSApply(hit, "//Hsp_qseq", xmlValue))
    hseq <- BStringSet(xpathSApply(hit, "//Hsp_hseq", xmlValue))
    names(qseq) <- paste0(query_id, "_Hsp_", num)
    names(hseq) <- paste0(id, "_Hsp_", num)
    hsp_list[[i]] <- list(qseq=qseq, hseq=hseq)
    names(hsp_list[[i]]) <- c("query", "hit")
    
    hit_table[[i]] <- 
      data.frame(stringsAsFactors=FALSE,
                 qid = query_id,                         # Id of query sequence
                 sid = id,                               # Id of database hit 
                 pident = 100*(identity/align_len),      # percent identity
                 length = align_len,                     # alignment length
                 mismatch = align_len - identity - gaps, # number of mismatches
                 gapopen = gapsOpen(qseq, hseq),                         # number of gap openings
                 qstart = query_from,
                 qend = query_to,
                 sstart = hit_from,
                 send = hit_to,
                 evalue = evalue,
                 bitscore = bit_score,
                 #### extended table ####
                 sdef = def,                      # subject definition line
                 sgi = gi,                        # subject gi
                 sacc = accn,                     # subject accession number
                 score = score,                   # raw score
                 nident = identity,               # number of identical matches
                 positive = positive,             # number of positive-scoring matches
                 gaps = gaps,                     # total number of gaps
                 ppos = 100*(positive/align_len), # percentage positive-scoring matches
                 qframe = query_frame,
                 sframe = hit_frame)
                          
  }
  
  hit_table <- do.call(rbind, hit_table)
  
  mlog.evalue <- with(hit_table, -log(evalue))
  mlog.evalue[is.infinite(mlog.evalue)] <- -log(1e-323)
  
  .blast(bitscore = hit_table$bitscore, evalue = hit_table$evalue,
         mlog.evalue = mlog.evalue, gi = hit_table$sgi,
         accession = hit_table$sacc, query = paste(query_id, query_def),
         params = params, hit_table = hit_table, hsp_list = hsp_list)
}


##' Extract fasta sequence from \code{\link{efetch-class}} objects
##' 
##' @param x an \code{\link{efetch-class}} object.
##' @param seqtype sequence type. One of 'DNA' (default), 'RNA', or 'AA'.
##' @param format Output format. One of 'Biostring' (default),
##' \code{\link[ape]{DNAbin}}, or a character vaector ('string').
##' 
##' @export
##' @examples
##'  ##
getFasta <- function (x,
                      seqtype=c("DNA","RNA","AA"),
                      format=c("Biostring", "DNAbin", "string"))
{
  
  ## default DNA
  seqtype <- match.arg(seqtype)
  
  ## default Biostring
  format <- match.arg(format)
  
  if (!is(x, "efetch"))
    stop(sprintf("x is of class %s. getFasta expects 'efetch'", sQuote(class(x))))
  
  if (!grepl(pattern="^>", x@data))
    stop("x does not appear to contain a valid fasta file")
  
  if (format == "Biostring") {
    stopifnot(require(Biostrings))
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
    stopifnot(require(ape))
    fasta <- ape::read.dna(file=textConnection(x@data), format="fasta")
    return(fasta)  
  }
  else if (format == "string") {
    fasta_split <- strsplit(x@data, "\n")[[1]]
    desc_idx <- which(grepl(pattern="^>", fasta_split))
    desc <- sub(">", "", fasta_split[desc_idx])
    fasta <- paste0(fasta_split[-desc_idx], collapse="")
    names(fasta) <- desc
    return(fasta)
  }
}

##' read a blast hit table
##' 
##' @param filepath File path.
##' 
##' @return A \code{\link{blast-class}} object.
##' 
##' @export
readBlastTable <- function (filepath)
{
  hit_table <- read.table(file=filepath, header=FALSE, sep="\t", as.is=TRUE,
                          col.names=c("qid", "sid", "pident",
                                      "length", "mismatch", "gapopen",
                                      "qstart", "qend", "sstart",
                                      "send","evalue","bitscore"),
                          colClasses=c("character", "character", "numeric",
                                       "integer", "integer", "integer",
                                       "integer", "integer", "integer",
                                       "integer", "numeric", "numeric"))
  if (dim(hit_table)[2] != 12)
    stop(sprintf("The BLAST file has %i columns but must have 12", dim(hit_table)[2]))
  
  ## parse subjectIds in 'hit_table'
  all_ids <- with(hit_table, strsplit(subjectIds, "\\|"))
  gi  <- vapply(all_ids, '[', 2, FUN.VALUE=character(1))
  source_tag <- vapply(all_ids, '[', 3, FUN.VALUE=character(1))
  accn <- ifelse(grepl("pdb", source_tag),
                 paste0(sapply(all_ids, '[', 4), "_", sapply(all_ids, '[', 5)),
                 sapply(all_ids, '[', 4))
  
  neg_log_evalue <- with(hit_table, -log(as.numeric(evalue)))
  neg_log_evalue[is.infinite(neg_log_evalue)] <- -log(1e-323)
  
  .blast(bitscore = as.numeric(hit_table$bitscore),
         evalue = as.numeric(hit_table$evalue),
         mlog.evalue = neg_log_evalue,
         gi = gi,
         accession = accn,
         hit_table = hit_table)
}








