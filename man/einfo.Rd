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
  See the official online documentation for NCBI's
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo}{EUtilities}
  for additional information.
}
\examples{
## get database statistics and search fields

# a list of all valid Entrez databases
databases <- einfo()
databases

# database statistics for protein
protein <- einfo(db="protein")
protein

# search fields for protein
p <- content(protein)
cbind(p$fields$Name, strtrim(p$fields$Description, 60))
}

