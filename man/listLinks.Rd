\name{listLinks}
\alias{listLinks}
\title{Retrieve links for an NCBI database}
\usage{
  listLinks(db)
}
\arguments{
  \item{db}{An NCBI database}
}
\value{
  A data frame
}
\description{
  Retrieve links for an NCBI database
}
\examples{
pml <- listLinks("pubmed")
   pml$Name
}

