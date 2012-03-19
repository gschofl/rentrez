\name{ecount}
\alias{ecount}
\title{Retrieve the number of records in an Entrez database matching a text
query}
\usage{
  ecount(term, db = "nuccore", ...)
}
\arguments{
  \item{term}{A valid NCBI search term.}

  \item{db}{An NCBI database (default = "pubmed").}

  \item{...}{Additional parameters passed on to
  \code{\link{esearch}}.}
}
\value{
  A numeric.
}
\description{
  Some additional details about this function
}
\examples{
## retrieve the number of records for a search term in a database
n <- ecount(term="Chlamydia psittaci", db="gene")
n
}
\seealso{
  \code{\link{esearch}}.
}

