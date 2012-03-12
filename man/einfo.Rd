\name{einfo}
\alias{einfo}
\title{Retrieve information about NCBI's databases}
\usage{
  einfo(db = NULL)
}
\arguments{
  \item{db}{\code{NULL} or a valid NCBI database name}
}
\value{
  ReturnValue
}
\description{
  If no database is provided \code{einfo} will return a
  list of databases available for querying. For specific
  databases, \code{einfo} provides the name, a description,
  the number indexed in the database, the date of the last
  update of the database, the fields and the available
  links to other Entrez databases.
}
\examples{
databases <- einfo()
}
\seealso{
  linkToSomewhere
}

