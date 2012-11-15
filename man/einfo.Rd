% Built by roxygen3 0.1
\docType{class}
\name{einfo-class}
\alias{einfo}
\alias{einfo-class}
\title{einfo}
\usage{
  einfo(db)
}
\arguments{
  \item{db}{A valid NCBI database name. If missing a list of all current NCBI
databases is returned.}
}

\value{
An \code{einfo} instance.
}
\description{
\dQuote{einfo} is a virtual S4 class that is extended by the 
\code{\linkS4class{einfoDbList}}, and \code{\linkS4class{einfoDb}} classes.

\code{einfo} retrieves information about each database in the NCBI Entrez
system. If no database is specified \code{einfo} will return a list of
currently available NCBI databases.
For specific databases, \code{einfo} returns the name, a description, the
number of records indexed in the database, the date of the last update of
the database, the fields and the available links from the database to
other Entrez databases.
}
\details{
See the documentation for the NCBI
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
  \item \code{\link{dbName}}
  \item \code{\link{description}}* (1 methods defined for subclasses)
  \item \code{\link{fields}}* (1 methods defined for subclasses)
  \item \code{\link{lastUpdate}}* (1 methods defined for subclasses)
  \item \code{\link{links}}* (1 methods defined for subclasses)
  \item \code{\link{menuName}}* (1 methods defined for subclasses)
  \item \code{\link{records}}* (1 methods defined for subclasses)
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
protein <- einfo("protein")
protein

# retrieve xml response
p <- content(protein, "xml")
p
}

\keyword{classes}
