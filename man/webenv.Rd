% Built by roxygen3 0.1
\docType{class}
\name{webenv-class}
\alias{webenv-class}
\title{webenv}
\description{
\dQuote{webenv} is an S4 class that provides a container for access
information to NCBI web environments.
}
\section{Class hierarchy for webenv}{
Super classes:
\itemize{
  \item \linkS4class{webOrId}
}

}

\section{Generics with methods for webenv}{
\itemize{
  \item \code{\link{show}}
}

}

\keyword{classes}
\section{Slots}{
\describe{
  \item{database}{Database from which the UIDs were retrieved.}

  \item{retStart}{The index of the first UID that is returned
by a call to \code{\link{esearch}}.}

  \item{retMax}{The number of UIDs out of the total number of
records that is returned by a call to \code{\link{esearch}}.}

  \item{count}{The total number of records matching a query.}

  \item{queryTranslation}{The search term as translated by the
Entrez search system.}

  \item{queryKey}{Integer query key returned by an esearch call
when \strong{usehistory} is set \code{TRUE}.}

  \item{webEnv}{Web environment string returned from an esearch
call when \strong{usehistory} is set \code{TRUE}}

}
}
