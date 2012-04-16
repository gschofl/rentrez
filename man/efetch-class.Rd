\name{efetch-class}
\alias{c,efetch-method}
\alias{efetch-class}
\alias{getSeq,efetch-method}
\alias{show,efetch-method}
\alias{write,efetch-method}
\title{efetch class}
\usage{
  .efetch(...)
}
\description{
  efetch is an S4 class that extends the
  \code{\link{eutil-class}}. This class provides a
  container for data retrived by calls to the NCBI EFetch
  utility.
}
\details{
  efetch objects have three slots in addition to the slots
  provided by basic \code{\link{eutil-class}} objects:
  \describe{ \item{database}{The name of the queried
  database.} \item{type}{A character vector specifying the
  record view returned, such as Abstract or MEDLINE from
  PubMed, or GenPept or FASTA from protein} \item{mode}{A
  character vector specifying the data format of the
  records returned, such as plain text, HMTL or XML} }
}
\seealso{
  \code{\link{efetch}} for generating calls to the NCBI
  EFetch utility.
}

