% Built by roxygen3 0.1
\docType{class}
\name{eutil-class}
\alias{eutil-class}
\title{eutil}
\description{
\dQuote{eutil} is an S4 class that is extended by the 
\code{\linkS4class{einfo}}, \code{\linkS4class{esearch}},
\code{\linkS4class{esummary}}, \code{\linkS4class{efetch}},
\code{\linkS4class{elink}}, \code{\linkS4class{epost}},
and \code{\linkS4class{egquery}} classes.
}
\details{
These classes provide containers for the data returned from calls to the
NCBI Entrez Utilities.
Detailed information about the EUtilities provided by NCBI is available
\href{http://www.ncbi.nlm.nih.gov/books/NBK25501/}{here}.
}
\section{Class hierarchy for eutil}{
Sub classes:
\itemize{
  \item \linkS4class{einfo}
  \item \linkS4class{esearch}
  \item \linkS4class{esummary}
  \item \linkS4class{efetch}
  \item \linkS4class{epost}
  \item \linkS4class{elink}
  \item \linkS4class{egquery}
  \item \linkS4class{einfoDbList}
  \item \linkS4class{einfoDb}
}

}

\section{Generics with methods for eutil}{
\itemize{
  \item \code{\link{[}}* (4 methods defined for subclasses)
  \item \code{\link{[[}}* (1 methods defined for subclasses)
  \item \code{\link{c}}* (1 methods defined for subclasses)
  \item \code{\link{content}} (4 methods defined for subclasses)
  \item \code{\link{count}}* (3 methods defined for subclasses)
  \item \code{\link{database}}* (5 methods defined for subclasses)
  \item \code{\link{dbName}}* (1 methods defined for subclasses)
  \item \code{\link{description}}* (1 methods defined for subclasses)
  \item \code{\link{docsum}}* (1 methods defined for subclasses)
  \item \code{\link{error}}
  \item \code{\link{fields}}* (1 methods defined for subclasses)
  \item \code{\link{idList}}* (2 methods defined for subclasses)
  \item \code{\link{lastUpdate}}* (1 methods defined for subclasses)
  \item \code{\link{length}}* (1 methods defined for subclasses)
  \item \code{\link{links}}* (1 methods defined for subclasses)
  \item \code{\link{linkSet}}* (1 methods defined for subclasses)
  \item \code{\link{menuName}}* (1 methods defined for subclasses)
  \item \code{\link{queryKey}}* (3 methods defined for subclasses)
  \item \code{\link{queryTranslation}}* (1 methods defined for subclasses)
  \item \code{\link{queryUrl}}
  \item \code{\link{records}}* (1 methods defined for subclasses)
  \item \code{\link{retmax}}* (1 methods defined for subclasses)
  \item \code{\link{retmode}}* (1 methods defined for subclasses)
  \item \code{\link{retstart}}* (1 methods defined for subclasses)
  \item \code{\link{rettype}}* (1 methods defined for subclasses)
  \item \code{\link{show}}* (7 methods defined for subclasses)
  \item \code{\link{webEnv}}* (3 methods defined for subclasses)
  \item \code{\link{write}}* (1 methods defined for subclasses)
}

* = methods only defined for subclasses
}

\keyword{classes}
\section{Slots}{
\describe{
  \item{url}{A character vector containing the query URL.}

  \item{error}{Any error or warning messages parsed from
the output of the call submitted to Entrez.}

  \item{content}{A \code{\linkS4class{raw}} vector holding the unparsed
contents of a request to Entrez.}

}
}
