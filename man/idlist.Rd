% Built by roxygen3 0.1
\docType{class}
\name{idlist-class}
\alias{idlist-class}
\title{idlist}
\description{
\dQuote{idlist} is an S4 class that provides a container for primary
UIDs used by NCBI.
}
\section{Class hierarchy for idlist}{
Super classes:
\itemize{
  \item \linkS4class{webOrId}
}

}

\section{Generics with methods for idlist}{
\itemize{
  \item \code{\link{[}}
  \item \code{\link{length}}
  \item \code{\link{show}}
}

}

\keyword{classes}
\section{Slots}{
\describe{
  \item{database}{Database from which the UIDs were retrieved}

  \item{retStart}{The index of the first UID that is returned
by a call to \code{\link{esearch}}.}

  \item{retMax}{The number of UIDs out of the total number of
records that is returned by a call to \code{\link{esearch}}.}

  \item{count}{The total number of records matching a query.}

  \item{queryTranslation}{The search term as translated by the
Entrez search system}

  \item{uid}{A character vector holding the retrieved UIDs.}

}
}
