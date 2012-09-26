% Built by roxygen3 0.1
\docType{class}
\name{einfo-class}
\alias{einfo}
\alias{einfo-class}
\title{einfo}
\usage{
  einfo(db = NULL)
}
\arguments{
  \item{db}{\code{NULL} or a valid NCBI database name}
}

\value{
An \code{einfo} object.
}
\description{
\dQuote{einfo} is a virtual S4 class that is extended by the 
\code{\linkS4class{einfoDbList}}, and \code{\linkS4class{einfoDb}} classes.

\code{einfo} rtrieves information about each database in the NCBI Entrez
system. If no database is specified \code{einfo} will return the current
list of NCBI databases available for querying.
For specific databases, \code{einfo} returns the name, a description, the
number of records indexed in the database, the date of the last update of
the database, the fields and the available links from the database to
other Entrez databases.
}
\details{
See the official online documentation for NCBI's
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo}{EUtilities}
for additional information.
}
\section{Class hierarchy for einfo}{
Sub classes:
\itemize{
  \item \linkS4class{einfoDbList}
  \item \linkS4class{einfoDb}
}
Super classes:
\itemize{
  \item \linkS4class{eutil}
}

}

\section{Generics with methods for einfo}{
\itemize{
  \item \code{\link{[}}* (1 methods defined for subclasses)
  \item \code{\link{content}}* (2 methods defined for subclasses)
  \item \code{\link{show}}
}

* = methods only defined for subclasses
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

\keyword{classes}
