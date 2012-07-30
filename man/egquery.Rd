\name{egquery}
\alias{egquery}
\title{Retrieve the number of records in all Entrez databases for a single
text query}
\usage{
  egquery(term)
}
\arguments{
  \item{term}{A valid Entrez text query.}
}
\value{
  An \code{\linkS4class{egquery}} object.
}
\description{
  \code{egquery} provides Entrez database counts for a
  global search.
}
\details{
  See the official online documentation for NCBI's
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EGQuery}{EUtilities}
  for additional information.
}
\examples{
## Perform a global Entrez Search

# Determine the number of records for "Chlamydia psittaci" in Entrez
egquery("Chlamydia psittaci")
}

