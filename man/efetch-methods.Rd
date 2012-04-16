\docType{methods}
\name{getSeq}
\alias{getSeq}
\title{Extract sequence information from \code{\link{efetch-class}} objects.}
\arguments{
  \item{x}{an \code{\link{efetch-class}} object.}

  \item{seqtype}{sequence type. One of 'DNA' (default),
  'RNA', or 'AA'.}

  \item{outfmt}{Output format. One of 'Biostring'
  (default), \code{\link[ape]{DNAbin}}, or a character
  vector ('String').}
}
\value{
  An object specified by \code{outfmt} is created.
}
\description{
  Extract sequence information from
  \code{\link{efetch-class}} objects.
}
\examples{
##
}

