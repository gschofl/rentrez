# content-generic --------------------------------------------------------

#' Extract content from an eutil.
#' 
#' @description These methods retrieve the data returned by a call to NCBI's
#' Entrez Utilities from the resulting \linkS4clas{eutil} instances.
#' As default, the methods will attempt to parse the contents into an
#' R object.
#' 
#' @details
#' 
#' @section efetch-methods
#' \emph{pubmed} records retrieved by \code{\link{efetch}} are parsed into
#' \link{bibentry} objects.
#' 
#' Records retrieved from the \emph{taxonomy} database are parsed into
#' \linkS4class{taxon} or \linkS4class{taxonList} objects.
#' 
#' FASTA Records retrieved from the \emph{protein} or \emph{nucleotide}
#' databases can be parsed into \linkS4class{AAStringSet},
#' \linkS4class{DNAStringSet}, \link[ape]{DNAbin}, or \code{character} vectors.
#' 
#' GenBank Records retrieved from the \emph{protein} or \emph{nucleotide}
#' databases are parsed into \linkS4class{gbRecord} objects.
#' 
#' @section esummary-methods 
#' Returns the Document Summaries retrieved by a call to
#' \code{link{esummary}} either as a data.frame or as a parsed XML tree.
#' 
#' @param x An \code{\linkS4class{eutil}} instance.
#' @param parse if \code{TRUE} the retrieved Document Summaries are 
#'  returned as a data.frame.
#' @param format (only \linkS4class{efetch} objects). If FASTA records are
#'  retrieved from the NCBI \emph{protein} or \emph{nuccore} databases the
#'  output format can be one of \linkS4class{AAStringSet} or
#'  \linkS4class{DNAStringSet} (default option 'Biostrings'),
#'  \link[ape]{DNAbin} (option: 'DNAbin'), or a \code{character} vector
#'  (option: 'String').
#' @param ... Further arguments passed on to methods. 
#' 
#' @rdname content
#' @export
#' @genericMethods
setGeneric("content", function(x, ...) standardGeneric("content"))


# eutil accessor generics ------------------------------------------------


#' @title Eutil accessors
#' 
#' @description
#' Access the various slots of \linkS4class{eutil} instances.
#' 
#' @param x An \code{\linkS4class{eutil}} instance.
#' @param ... Further arguments passed on to methods. 
#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("error", function(x, ...) standardGeneric("error"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("query",  function(x, ...) standardGeneric("query"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("retmode", function(x, ...) standardGeneric("retmode"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("rettype", function(x, ...) standardGeneric("rettype"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("database", function(x, ...) standardGeneric("database"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("retmax", function(x, ...) standardGeneric("retmax"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("retstart", function(x, ...) standardGeneric("retstart"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("count", function(x, ...) standardGeneric("count"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("queryTranslation", function(x, ...) standardGeneric("queryTranslation"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("uid", function(x, ...) standardGeneric("uid"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("webEnv", function(x, ...) standardGeneric("webEnv"))

#' @rdname eutil-accessors
#' @export
#' @genericMethods
setGeneric("queryKey", function(x, ...) standardGeneric("queryKey"))


# taxon accessor generics ------------------------------------------------


#' @title Taxon accessors
#' 
#' @description
#' Access the various slots of \linkS4class{taxon} or \linkS4class{taxonList}
#' instances.
#' 
#' @param x A \linkS4class{taxon} or \linkS4class{taxonList}.
#' @param ... Further arguments passed on to methods. 
#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("taxId", function(x, ...) standardGeneric("taxId"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("sciName", function(x, ...) standardGeneric("sciName"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("taxRank", function(x, ...) standardGeneric("taxRank"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("synonym", function (x, ...) standardGeneric("synonym"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("authority", function (x, ...) standardGeneric("authority"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("lineage", function(x, ...) standardGeneric("lineage"))

#' @rdname taxon-accessors
#' @export
#' @genericMethods
setGeneric("parent", function (x, ...) standardGeneric("parent"))

