\name{einfo}
\alias{einfo}
\title{Retrieve information about each database in the NCBI Entrez system}
\usage{
  einfo(db = NULL)
}
\arguments{
  \item{db}{\code{NULL} or a valid NCBI database name}
}
\value{
  An \code{\link{einfo-class}} object.
}
\description{
  If no database is provided \code{einfo} will return the
  current list of NCBI databases available for querying.
  For specific databases, \code{einfo} provides the name, a
  description, the number of records indexed in the
  database, the date of the last update of the database,
  the fields and the available links from the database to
  other Entrez databases.
}
\details{
  See the online documentation at
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo}
  for additional information.
}
\examples{
databases <- einfo()
databases

pubmed <- einfo(db="pubmed")
pubmed$fields[,c("Name", "Description")]
}

