\name{listFields}
\alias{listFields}
\title{Retrieve field codes for an NCBI database}
\usage{
  listFields(db, all = FALSE)
}
\arguments{
  \item{db}{An NCBI database}

  \item{all}{if \code{FALSE} only field names, full names,
  and field descriptions are returned.}
}
\value{
  A data frame
}
\description{
  Retrieve field codes for an NCBI database
}
\examples{
pmf <- listFields("pubmed")
   pmf$Name
}

