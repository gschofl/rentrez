\docType{package}
\name{rentrez-package}
\alias{rentrez-package}
\title{The rentrez package provides an interface to NCBI's
EUtils/Entrez server.}
\description{
  NCBI provides the EUtils web service for software to
  query the Entrez databases directly, rather than through
  the web interface. For more information see
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25500/}.
}
\details{
  This package provides an interface to construct query
  URLs and parse query results.

  Important functions: \code{\link{einfo}}: Provides field
  names, term counts, last update, and available updates
  for each database.  \code{\link{esearch}}: Searches and
  retrieves primary UIDs (for use in \code{esummary} and
  \code{efetch}) \code{\link{esummary}}: Retrieves document
  summaries (DocSums) from a list of primary UIDs.
}
\author{
  Gerhard Schöfl \email{gschofl@yahoo.de}
}
\references{
  \url{www.website.de}
}
\keyword{package}

