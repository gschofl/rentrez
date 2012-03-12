\name{getCount}
\alias{getCount}
\title{Retrieve the number of hits for a query.}
\usage{
  getCount(term, db = "pubmed", silent = FALSE, ...)
}
\arguments{
  \item{term}{A valid NCBI search term.}

  \item{db}{An NCBI database (default = "pubmed").}

  \item{silent}{If \code{TRUE} suppress messages.}

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
getCount('\\"gene expression\\"[title]')
}
\seealso{
  \code{\link{esearch}}.
}

