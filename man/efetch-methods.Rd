\docType{methods}
\name{parse}
\alias{parse}
\title{Parse \code{\link{efetch}} retrived records into R data structures}
\arguments{
  \item{x}{an \code{\link{efetch-class}} object.}

  \item{seqtype}{sequence type. 'DNA' (default) or 'AA'.}

  \item{outfmt}{Output format. 'Biostring' (default),
  \code{\link[ape]{DNAbin}}, or character vector
  ('String').}
}
\value{
  An object specified by \code{outfmt} is created.
}
\description{
  Parse \code{\link{efetch}} retrived records into R data
  structures
}

