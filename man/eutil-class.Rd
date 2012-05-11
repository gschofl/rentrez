\name{eutil-class}
\alias{eutil-class}
\alias{eutil,eutil-method}
\alias{$,eutil-method}
\alias{names,eutil-method}
\title{eutil class}
\usage{
  .eutil(...)
}
\description{
  eutil is an S4 class that is extended by the
  \code{\link{einfo-class}}, \code{\link{esearch-class}},
  \code{\link{esummary-class}}, \code{\link{efetch-class}},
  and \code{\link{epost-class}} classes.
}
\details{
  These classes provide containers for the data returned
  from calls to the NCBI Entrez Utilities. Detailed
  information about the EUtilities provided by NCBI is
  available
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25501/}{here}.

  Eutil class objects have three slots: \describe{
  \item{url}{A character vector containing the URL
  submitted to Entrez.} \item{error}{Any error messages
  parsed from the output of the call submitted to Entrez.}
  \item{data}{An
  \code{\link[XML]{XMLInternalDocument-class}} or character
  vector holding the unparsed output from the call
  submitted to Entrez.} }
}

