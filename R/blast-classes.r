
# Blast-Classes -------------------------------------------------------

##' @include utils.r
##' @include blast-utils.r
##' @include eutil-classes.r
NULL

##' blastReport class
##'
##' blastReport is an S4 class that provides a container for data retrived
##' by calls to the NCBI Blast utility.
##' 
##' blastReport objects have eight slots:
##' \describe{
##'   \item{program}{}
##'   \item{version}{}
##'   \item{reference}{}
##'   \item{db}{}
##'   \item{query}{}
##'   \item{params}{}
##'   \item{stats}{}
##'   \item{hits}{}
##' }
##' 
##' @param ... Slots for \sQuote{blastReport} objects.
##' @name blastReport-class
##' @rdname blastReport-class
##' @exportClass blastReport
##' @aliases show,blastReport-method
##' @aliases hits,blastReport-method
.blastReport <-
  #### blastReport-class ####
  setClass("blastReport",
           representation(program = "character",
                          version = "character",
                          reference = "character",
                          db = "character",
                          iter_num = "integer",
                          query = "list",
                          hits = "list",
                          params = "list",
                          stats = "list",
                          message = "character"),
           prototype(program = NA_character_,
                     version = NA_character_,
                     reference = NA_character_,
                     db = NA_character_,
                     iter_num = NA_integer_,
                     query = list(),
                     hits = list(),
                     params = list(),
                     stats = list(),
                     message = NA_character_))

##' hsp class
##'
##' @keywords internal
##' @name hsp-class
##' @rdname hsp-class
.hsp <- 
  #### hsp-class ####
  setClass("hsp",
           representation(
             num = "integer",          # HSP numbers
             bit_score = "numeric",    # bit scores of HSP
             score =  "integer",       # scores of HSPs
             evalue = "numeric",       # e-value
             query_from = "integer",   # start of HSPs in query
             query_to = "integer",     # end of HSPs in query
             hit_from = "integer",     # start of HSPs in subject
             hit_to = "integer",       # end of HSPs in subect
             pattern_from = "integer", # start of PHI-BLAST pattern
             pattern_to = "integer",   # end of PHI-BLAST pattern
             query_frame = "integer",  # translation frame of query
             hit_frame = "integer",    # translation frame of subject
             identity = "integer",     # numbers of identities
             positive = "integer",     # numbers of positives
             gaps = "integer",         # numbers of gaps
             align_len = "integer",    # lengths of alignments
             density = "numeric",      # score density
             qseq = "XStringSet",      # Alignment string for query (with gaps)
             hseq = "XStringSet",      # Alignment string for subject (with gaps)
             midline = "character",    # formatting middle line
             percent_identity = "numeric", # '-m 8' format output
             mismatch_count = "integer"    # '-m 8' format  output
             ))

##' hit class
##'
##' @keywords internal
##' @name hit-class
##' @rdname hit-class
##' @aliases show,hit-method
.hit <- 
  #### hit-class ####
  setClass("hit",
           representation(
             num = "integer",         # hit number
             id = "list",             # SeqIds
             desc = "list",           # Description line
             accn = "character",       # accession number
             len = "integer",         # length of subject
             hsp = "hsp"))            # hit HSPs

##' @export
setMethod("show",
          #### show-method, blastReport ####
          signature(object = "blastReport"),
          function (object) {
            ## Header
            def_pos <- which(names(object@query) ==  "def")
            def_line <- deparseDeflines(list(object@query[1:def_pos - 1]),
                                        object@query[3])
            query <- linebreak(sprintf("%s (%s letters)",
                                       def_line, object@query[["len"]]),
                               offset = 10)     
            cat(sprintf("Query:    %s\nProgram:  %s\nDatabase: %s\n\n",
                        query, sQuote(object@version), sQuote(object@db)))
            cat(paste0("Accession      ",
                       format("Description", width=ceiling(getOption("width")*0.5)),
                       " bit score", "  evalue\n"))
            invisible(
              lapply(object@hits, function (x) {
                cat(paste(format(x@accn, width=14),
                          format(
                            strtrim(deparseDeflines(x@id, x@desc)[[1]],
                                    width=floor(getOption("width")*0.5)),
                            width=ceiling(getOption("width")*0.5)),
                          format(x@hsp@bit_score, digits=4, width=9),
                          format(x@hsp@evalue, scientific=TRUE, digits=2, width=8),
                          "\n"))
              }))
            
            return(invisible(NULL))
          })

##' @export
setMethod("show",
          #### show-method, hit ####
          signature(object = "hit"),
          function (object) {
            
            first_line <-
              linebreak(deparseDeflines(object@id, object@desc), offset=5)         

            second_line <- 
              sprintf("Score: %s bits (%s), Expect: %s,",
                      round(object@hsp@bit_score, 1),
                      object@hsp@score,
                      format(object@hsp@evalue, scientific=TRUE, digits=2))

            third_line <- 
              sprintf("Identities: %s/%s (%s%%), Positives: %s/%s (%s%%), Gaps: %s/%s (%s%%)",
                      object@hsp@identity, object@hsp@align_len,
                      round(100*object@hsp@identity/object@hsp@align_len, 0),
                      object@hsp@positive, object@hsp@align_len,
                      round(100*object@hsp@positive/object@hsp@align_len, 0),
                      object@hsp@gaps, object@hsp@align_len,
                      round(100*object@hsp@gaps/object@hsp@align_len, 0))

            q_start <- min(c(object@hsp@query_from, object@hsp@query_to))
            q_rev <- if (object@hsp@query_from > object@hsp@query_to) TRUE else FALSE
            h_start <- min(c(object@hsp@hit_from, object@hsp@hit_to))
            h_rev <- if (object@hsp@hit_from > object@hsp@hit_to) TRUE else FALSE
            
            cat(sprintf("\nHit: %s (Length = %s)", first_line, object@len))
            cat(sprintf("\n\n%s", linebreak(second_line)))
            cat(sprintf("\n%s\n", linebreak(third_line)))
            cat(sprintf("\n%s\n",
                        wrapAln(toString(object@hsp@qseq), object@hsp@midline, toString(object@hsp@hseq),
                        prefix=c("Query", "", "Spjct"),
                        start=c(q_start, NA, h_start),
                        reverse=c(q_rev, FALSE, h_rev))))
          })


# seqs <- list(toString(object@hsp@qseq), object@hsp@midline, toString(object@hsp@hseq))

##' Accessor methods for blast records
##' 
##' @param x A \code{\link{blastReccord-class}} object.
##' @param ... Additional arguments
##' 
##' @rdname blastReport-method
##' @docType methods
##' @export
setGeneric("hits",
           #### hits-generic ####
           function (x, ...) {
             standardGeneric("hits")
             })

##' @export
setMethod("hits",
          #### hits-method ####
          signature="blastReport",
          function (x) {
            return(x@hits)
          })

##' Accessor methods for blast records
##' 
##' @usage getId(x, db="gi")
##' 
##' @param x A blastReport object.
##' @param db Database tag (e.g.: 'gi', 'gb', 'emb', 'ref', ...)
##' 
##' @return Accession numbers as specified by \code{db} for each hit.
##' 
##' @rdname blastReport-method
##' @docType methods
##' @export
setGeneric("getId",
           function (x, db="gi", ...) {
             #### getId-generic ####
             standardGeneric("getId")
           })

##' @export
setMethod("getId",
          #### getId-method ####
          signature="blastReport",
          function (x, db, ...) {
            lapply(x@hits, function (x) getId(x, db)) 
          })

##' @export
setMethod("getId",
          #### getId-method ####
          signature="hit",
          function (x, db, ...) {
            id <- lapply(x@id, "[[", db)
            id[vapply(id, is.null, logical(1))] <- NA_character_
            unlist(id)
          })


##' blastTable class
##' 
##' blastTable is an S4 class that provides a container for data retrived
##' by calls to the NCBI Blast utility.
##' 
##' blastReport objects have ten slots:
##' \describe{
##'   \item{program}{}
##'   \item{version}{}
##'   \item{reference}{}
##'   \item{db}{}
##'   \item{bit_score}{}
##'   \item{evalue}{}
##'   \item{mlog.evalue}{}
##'   \item{accession}{}
##'   \item{gi}{}
##'   \item{table}{}
##' }
##' 
##' @param ... Slots for 'blastTable' objects.
##' @name blastTable-class
##' @rdname blastTable-class
##' @exportClass blastTable
##' @aliases show,blastReport-method
##' @aliases $,blastReport-method
##' @aliases [,blastReport-method
##' @aliases [[,blastReport-method
##' @aliases names,blastReport-method
.blastTable <-
  #### blastTable-class ####
  setClass("blastTable",
           representation(program = "character",
                          query = "character",
                          db = "character",
                          bitscore = "numeric",
                          evalue = "numeric",
                          mlog.evalue = "numeric",
                          accession = "character",
                          gi = "character",
                          table = "data.frame"),
           prototype(program = NA_character_,
                     query = NA_character_,
                     db = NA_character_,
                     bitscore = NA_real_,
                     evalue = NA_real_,
                     mlog.evalue = NA_real_,
                     accession = NA_character_,
                     gi = NA_character_,
                     table = data.frame()))


##' @export
setMethod("show",
          #### show-method, blastTable ####
          signature(object = "blastTable"),
          function (object) {
            ## Header
            cat(sprintf("Query:    %s\nProgram:  %s\nDatabase: %s\n\n",
                        linebreak(object@query, offset=10),
                        sQuote(object@program), sQuote(object@db)))
            print(object@table)
            return(invisible(NULL))
          })

##' @export
setMethod("$",
          #### $-method, blastTable ####
          signature(x = "blastTable"),
          function(x, name) {
            slot(x, "table")[[name]]
          })

##' @export
setMethod("[",
          #### [-method, blastTable ####
          signature(x = "blastTable"),
          function (x, i, j, ..., drop = TRUE) {
            slot(x, "table")[i,j,...]
          })

##' @export
setMethod("[[",
          #### [[-method, blastTable ####
          signature(x = "blastTable"),
          function(x, i) {
            slot(x, "table")[[i]]
          })

##' @export
setMethod("names",
          #### names-method, blastTable ####
          signature(x = "blastTable"),
          function(x) {
            names(slot(x, "table"))
          })
