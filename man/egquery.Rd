% Built by roxygen3 0.1
\docType{class}
\name{egquery-class}
\alias{egquery}
\alias{egquery-class}
\title{egquery}
\usage{
  egquery(term)
}
\arguments{
  \item{term}{A valid Entrez text query.}
}

\value{
An \code{egquery} instance.
}
\description{
\dQuote{egquery} is an S4 class that provides a container for data
retrived by calls to the NCBI EGQuery utility.

\code{egquery} retrieves the number of records in all Entrez databases for
a single text query
}
\details{
See the official online documentation for NCBI's
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EGQuery}{EUtilities}
for additional information.
}
\section{Class hierarchy for egquery}{
Super classes:
\itemize{
  \item \linkS4class{eutil}
}

}

\section{Generics with methods for egquery}{
\itemize{
  \item \code{\link{show}}
}

}

\examples{
## Perform a global Entrez Search

# Determine the number of records for "Chlamydia psittaci" in Entrez
egquery("Chlamydia psittaci")
}

\keyword{classes}
\section{Slots}{
\describe{
  \item{url}{A character vector containing the query URL.}

  \item{error}{Any error or warning messages parsed from
the output of the call submitted to Entrez.}

  \item{content}{An \code{\linkS4class{XMLInternalDocument}} object or
a character vector holding the unparsed output from the call
submitted to Entrez.}

  \item{term}{The search term passed on to the EGQuery Utility.}

  \item{count}{A data frame with the following columns:
\code{dbName}, \code{menuName}, \code{count}, and \code{status}.}

}
}
