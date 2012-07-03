### Blast parser ###########################################################
##' @include utils.r
##' @include blast-utils.r
##' @include blast-classes.r
##' @include eutil-classes.r
NULL

##' Parse xml blast output
##' 
##' @param blast_output Blast output in XML format
##' (File or character vector)
##' 
##' @importFrom stringr str_split
##' @importFrom stringr str_split_fixed
##' @importFrom stringr str_replace_all
##' @importFrom Biostrings BStringSet
##' @importFrom Biostrings BString
##' 
##' @return A \code{\link{blastReport-class}} object.
##' 
##' @export
parseBlastXml <- function (blast_output)
{
  ## BlastOutput
  doc <- xmlRoot(xmlParse(blast_output))
  program <- xpathSApply(doc, "//BlastOutput_program", xmlValue)
  version <- xpathSApply(doc, "//BlastOutput_version", xmlValue)
  reference <- xpathSApply(doc, "//BlastOutput_reference", xmlValue)
  db <- xpathSApply(doc, "//BlastOutput_db", xmlValue)
  iter_num <- as.integer(xpathSApply(doc, "//BlastOutput_iter-num", xmlValue))
  message <- as.character(xpathSApply(doc, "//BlastOutput_message", xmlValue))
  
  ## BlastOutput_query
  qid <- parseDeflines(xpathSApply(doc, "//Iteration_query-ID", xmlValue))$id
  qdef <- xpathSApply(doc, "//Iteration_query-def", xmlValue)
  qlen <- xpathSApply(doc, "//Iteration_query-len", xmlValue)
  qseq <- xpathSApply(doc, "//Iteration_query-seq", xmlValue) # optional
  query <- merge(qid[[1L]], list(
    def=qdef,
    len=as.integer(qlen),
    seq=if (length(qseq) > 0L) {BString(qseq)} else {NULL}
    ))
  
  ## BlastOutput/BlastOutput_param/Parameters
  params <- xpathApply(doc, "//Parameters/*", xmlValue)
  names(params) <-
    gsub("-", "_", 
      sapply(xpathApply(doc, "//Parameters/*", xmlName), function (x) {
        strsplit(x, "_")[[1L]][2]
      }))
  
  ## BlastOutput/BlastOutput_Iterations//Statistics
  stats <- xpathApply(doc, "//Statistics/*", function (x) {
    as.numeric(xmlValue(x))
  })
  names(stats) <-
    gsub("-", "_", 
         sapply(xpathApply(doc, "//Statistics/*", xmlName), function (x) {
           strsplit(x, "_")[[1L]][2]
         }))

  ## Hits
  hits <- getNodeSet(doc, "//Hit")
  hit_list <- list()
  for (i in seq_along(hits)) {
    hit <- xmlDoc(hits[[i]]) 
    ## parse HSPs
    hsp_obj <- .hsp(num=as.integer(xpathSApply(hit, "//Hsp_num", xmlValue)),
                    bit_score=as.numeric(xpathSApply(hit, "//Hsp_bit-score", xmlValue)),
                    score=as.integer(xpathSApply(hit, "//Hsp_score", xmlValue)),
                    evalue=as.numeric(xpathSApply(hit, "//Hsp_evalue", xmlValue)),
                    query_from=as.integer(xpathSApply(hit, "//Hsp_query-from", xmlValue)),
                    query_to=as.integer(xpathSApply(hit, "//Hsp_query-to", xmlValue)),
                    hit_from=as.integer(xpathSApply(hit, "//Hsp_hit-from", xmlValue)),
                    hit_to=as.integer(xpathSApply(hit, "//Hsp_hit-to", xmlValue)),
                    pattern_from=as.integer(xpathSApply(hit, "//Hsp_pattern-from", xmlValue)),
                    pattern_to=as.integer(xpathSApply(hit, "//Hsp_pattern-to", xmlValue)),
                    query_frame=as.integer(xpathSApply(hit, "//Hsp_query-frame", xmlValue)),
                    hit_frame=as.integer(xpathSApply(hit, "//Hsp_hit-frame", xmlValue)),
                    identity=as.integer(xpathSApply(hit, "//Hsp_identity", xmlValue)),
                    positive=as.integer(xpathSApply(hit, "//Hsp_positive", xmlValue)),
                    gaps=as.integer(xpathSApply(hit, "//Hsp_gaps", xmlValue)),
                    density=as.numeric(xpathSApply(hit, "//Hsp_density", xmlValue)),
                    align_len=as.integer(xpathSApply(hit, "//Hsp_align-len", xmlValue)),
                    qseq={
                      qseq <- BStringSet(xpathSApply(hit, "//Hsp_qseq", xmlValue))
                      names(qseq) <- paste0("hsp", xpathSApply(hit, "//Hsp_num", xmlValue))
                      qseq
                    },
                    hseq={
                      hseq <- BStringSet(xpathSApply(hit, "//Hsp_hseq", xmlValue))
                      names(hseq) <- paste0("hsp", xpathSApply(hit, "//Hsp_num", xmlValue))
                      hseq
                    },
                    midline=as.character(xpathSApply(hit, "//Hsp_midline", xmlValue)),
                    percent_identity=as.numeric(xpathSApply(hit, "//Hsp_percent-identity", xmlValue)),
                    mismatch_count=as.integer(xpathSApply(hit, "//Hsp_mismatch-count", xmlValue)))
    
    ## parse hits
    id <- paste(xpathSApply(hit, "//Hit_id", xmlValue),
                xpathSApply(hit, "//Hit_def", xmlValue))
    id <- parseDeflines(defline=str_split(id, " >")[[1L]])
    hit_obj <- .hit(num=as.integer(xpathSApply(hit, "//Hit_num", xmlValue)),
                    id=id$id,
                    desc=id$desc,
                    accn=as.character(xpathSApply(hit, "//Hit_accession", xmlValue)),
                    len=as.integer(xpathSApply(hit, "//Hit_len", xmlValue)),
                    hsp=hsp_obj)
    
    hit_list[[i]] <- hit_obj
  }
  
  .blastReport(program=program, version=version, reference=reference,
               db=db, query=query, iter_num=iter_num, hits=hit_list,
               params=params, stats=stats, message=message)  
}

##' read a blast hit table
##' 
##' @param blast_output Blast output in XML format
##' (File or character vector)
##' 
##' @return A \code{\link{blastReport-class}} object.
##' 
##' @export
parseBlastTabular <- function (blast_output)
{
  if (is.character(blast_output) && file.exists(blast_output)) {
    cf <- count.fields(blast_output, sep="\t", comment.char="#")
    file_path <- file(blast_output, open="r")
  } else {
    cf <- count.fields(textConnection(as.character(blast_output)),
                       sep="\t", comment.char="#")
    file_path <- textConnection(as.character(blast_output))
  }

  if (!all(cf == 12)) {
    stop(sprintf("Malformed blast table. %s columns in row %s.\n",
                 sQuote(cf[cf > 12]), sQuote(which(cf > 12))))
  }
  
  hasComment <- TRUE
  comStr <- NULL
  while (hasComment) {
    line <- readLines(file_path, n=1)
    if (hasComment <- str_detect(line, "^#")) {
      comStr <- c(comStr, str_match(line, "[^(# *)].*"))
    }
  }
  
  pushBack(line, connection=file_path)  
  hit_table <- 
    read.table(file_path, header=FALSE, sep="\t",
               as.is=TRUE, nrows=length(cf),
               col.names=c("qid", "sid", "pident",
                           "length", "mismatch", "gapopen",
                           "qstart", "qend", "sstart",
                           "send","evalue","bitscore"),
               colClasses=c("character", "character", "numeric",
                            "integer", "integer", "integer",
                            "integer", "integer", "integer",
                            "integer", "numeric", "numeric"))

  ## parse subjectIds in 'hit_table'
  all_ids <- with(hit_table, strsplit(sid, "\\|"))
  gi  <- vapply(all_ids, '[', 2, FUN.VALUE=character(1))
  source_tag <- vapply(all_ids, '[', 3, FUN.VALUE=character(1))
  accn <- ifelse(grepl("pdb", source_tag),
                 paste0(sapply(all_ids, '[', 4), "_", sapply(all_ids, '[', 5)),
                 sapply(all_ids, '[', 4))
  if (all(is.na(accn)))
    accn <- hit_table[["sid"]]
  
  neg_log_evalue <- with(hit_table, -log(as.numeric(evalue)))
  neg_log_evalue[is.infinite(neg_log_evalue)] <- -log(1e-323)
  
  if (!is.null(comStr) && length(comStr) == 5) {
    program <- comStr[1]
    query <- str_split_fixed(comStr[2], "Query: ", 2)[,2]
    db <- str_split_fixed(comStr[3], "Database: ", 2)[,2]
  } else {
    program <- query <- db <- NA_character_
  }
  
  close(file_path)
  
  .blastTable(program = program, db = db, query = query,
              bitscore = as.numeric(hit_table[["bitscore"]]),
              evalue = as.numeric(hit_table[["evalue"]]),
              mlog.evalue = neg_log_evalue,
              gi = gi,
              accession = accn,
              table = hit_table)
}


# gapsOpen <- function (s1, s2)
# {
#   length(gregexpr("-+", s1)[[1L]]) + length(gregexpr("-+", s2)[[1L]])
# }

# {   
#     hit_table[[i]] <- 
#       data.frame(stringsAsFactors=FALSE,
#                  qid = query_id,                         # Id of query sequence
#                  sid = id,                               # Id of database hit 
#                  pident = 100*(identity/align_len),      # percent identity
#                  length = align_len,                     # alignment length
#                  mismatch = align_len - identity - gaps, # number of mismatches
#                  gapopen = gapsOpen(qseq, hseq),                         # number of gap openings
#                  qstart = query_from,
#                  qend = query_to,
#                  sstart = hit_from,
#                  send = hit_to,
#                  evalue = evalue,
#                  bitscore = bit_score,
#                  ### extended table ###
#                  sdef = def,                      # subject definition line
#                  sgi = gi,                        # subject gi
#                  sacc = accn,                     # subject accession number
#                  score = score,                   # raw score
#                  nident = identity,               # number of identical matches
#                  positive = positive,             # number of positive-scoring matches
#                  gaps = gaps,                     # total number of gaps
#                  ppos = 100*(positive/align_len), # percentage positive-scoring matches
#                  qframe = query_frame,
#                  sframe = hit_frame)
#                           
#   }
