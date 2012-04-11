\name{getFasta}
\alias{getFasta}
\title{Extract fasta sequence from \code{\link{efetch-class}} objects}
\usage{
  getFasta(x, seqtype = c("DNA", "RNA", "AA"),
    format = c("Biostring", "DNAbin", "string"))
}
\arguments{
  \item{x}{an \code{\link{efetch-class}} object.}

  \item{seqtype}{sequence type. One of 'DNA' (default),
  'RNA', or 'AA'.}

  \item{format}{Output format. One of 'Biostring'
  (default), \code{\link[ape]{DNAbin}}, or a character
  vaector ('string').}
}
\description{
  Extract fasta sequence from \code{\link{efetch-class}}
  objects
}
\examples{
##
}

