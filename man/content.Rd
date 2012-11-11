% Built by roxygen3 0.1
\docType{methods}
\name{content}
\alias{content}
\alias{content,esummary-method}
\title{Extract content from an eutil.}
\usage{
  content(x, ...)

  \S4method{content}{esummary}(x, ...)
}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} instance.}

  \item{parse}{if \code{TRUE} the retrieved Document Summaries are 
 returned as a data.frame.}

  \item{format}{(only \linkS4class{efetch} objects). If FASTA records are
 retrieved from the NCBI \emph{protein} or \emph{nuccore} databases the
 output format can be one of \linkS4class{AAStringSet} or
 \linkS4class{DNAStringSet} (default option 'Biostrings'),
 \link[ape]{DNAbin} (option: 'DNAbin'), or a \code{character} vector
 (option: 'String').}

  \item{...}{Further arguments passed on to methods.}

  \item{x}{An \code{\linkS4class{eutil}} instance.}

  \item{...}{Further arguments passed on to methods.}
}

\description{
These methods retrieve the data returned by a call to NCBI's
Entrez Utilities from the resulting \linkS4clas{eutil} instances.
As default, the methods will attempt to parse the contents into an
R object.
}
\details{

}
\section{efetch-methods
\emph{pubmed} records retrieved by \code{\link{efetch}} are parsed into
\link{bibentry} objects.

Records retrieved from the \emph{taxonomy} database are parsed into
\linkS4class{taxon} or \linkS4class{taxonList} objects.

FASTA Records retrieved from the \emph{protein} or \emph{nucleotide}
databases can be parsed into \linkS4class{AAStringSet},
\linkS4class{DNAStringSet}, \link[ape]{DNAbin}, or \code{character} vectors.

GenBank Records retrieved from the \emph{protein} or \emph{nucleotide}
databases are parsed into \linkS4class{gbRecord} objects.}{

}

\section{esummary-methods 
Returns the Document Summaries retrieved by a call to
\code{link{esummary}} either as a data.frame or as a parsed XML tree.}{

}

\section{Method signatures for generic content}{
\itemize{
  \item \linkS4class{efetch}
  \item \linkS4class{einfoDb}
  \item \linkS4class{einfoDbList}
  \item \linkS4class{elink}
  \item \linkS4class{epost}
  \item \linkS4class{esearch}
  \item \linkS4class{esummary}
}

}

\keyword{methods}
