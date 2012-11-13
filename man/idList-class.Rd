% Built by roxygen3 0.1
\docType{class}
\name{idList-class}
\alias{idList-class}
\title{A list of Entrez UIDs that match a text query}
\description{
A list of Entrez UIDs that match a text query
}
\section{Generics with methods for idList}{
\itemize{
  \item \code{\link{[}}
  \item \code{\link{count}}
  \item \code{\link{database}}
  \item \code{\link{idList}}
  \item \code{\link{length}}
  \item \code{\link{queryKey}}
  \item \code{\link{queryTranslation}}
  \item \code{\link{retmax}}
  \item \code{\link{retstart}}
  \item \code{\link{show}}
  \item \code{\link{webEnv}}
}

}

\keyword{classes}
\section{Slots}{
\describe{
  \item{database}{Database from which the UIDs were retrieved.}

  \item{retstart}{The index of the first UID that is returned.}

  \item{retmax}{The number of UIDs out of the total number of
records that is returned.}

  \item{count}{The total number of records matching a query.}

  \item{queryTranslation}{The search term as translated by the
Entrez search system.}

  \item{queryKey}{An object of class \linkS4class{numeric}}

  \item{webEnv}{An object of class \linkS4class{character}}

  \item{idList}{A list of primary UIDs.}

}
}
