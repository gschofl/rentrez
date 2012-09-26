\name{eutil-class}
\alias{error,eutil-method}
\alias{eutil-class}
\alias{eutil,eutil-method}
\alias{query,eutil-method}
\title{\dQuote{eutil} class}
\usage{
  .eutil(...)

  \S4method{error}{eutil}(x, ...)

  \S4method{query}{eutil}(x, ...)
}
\arguments{
  \item{...}{arguments passed to the constructor method}

  \item{x}{an \code{\linkS4class{eutil}} object.}
}
\description{
  eutil is an S4 class that is extended by the
  \code{\linkS4class{einfo}}, \code{\linkS4class{esearch}},
  \code{\linkS4class{esummary}},
  \code{\linkS4class{efetch}}, \code{\linkS4class{elink}},
  \code{\linkS4class{epost}}, and
  \code{\linkS4class{egquery}} classes.
}
\details{
  These classes provide containers for the data returned
  from calls to the NCBI Entrez Utilities. Detailed
  information about the EUtilities provided by NCBI is
  available
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25501/}{here}.
}
\section{Slots}{
  \describe{ \item{\code{url}:}{A character vector
  containing the query URL.} \item{\code{error}:}{Any error
  or warning messages parsed from the output of the call
  submitted to Entrez.} \item{\code{content}:}{An
  \code{XMLInternalDocument} object or a character vector
  holding the unparsed output from the call submitted to
  Entrez.} }
}

