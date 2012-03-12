\name{getUIDs}
\alias{getUIDs}
\title{Retrieve a list of UIDs for a query}
\usage{
  getUIDs(term, db = "pubmed", silent = FALSE,
    retmax = getCount(term, db, TRUE, ...), ...)
}
\arguments{
  \item{term}{A valid NCBI search term.}

  \item{db}{An NCBI database (default = "pubmed").}

  \item{silent}{If \code{TRUE} suppress messages.}

  \item{retmax}{Maximum number of UIDs retrieved.
  (defaults to number of hits)}

  \item{...}{Additional parameters passed on to
  \code{\link{esearch}}.}
}
\value{
  A numeric vector of UIDs.
}
\description{
  Some additional details about this function
}
\examples{
##
}
\seealso{
  \code{\link{esearch}}.
}

