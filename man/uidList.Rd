% Built by roxygen3 0.1
\docType{class}
\name{uidList-class}
\alias{uid-class}
\alias{uidList-class}
\alias{webenv-class}
\title{[DEV] A list of Entrez UIDs that match a text query}
\description{
This function is aimed primarily at developers extending this package.
You shouldn't need to use it for everyday operation.

A list of Entrez UIDs that match a text query
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

  \item{uid}{A list of primary UIDs or a Query Key and Web Environment
string.}

  \item{queryKey}{}

  \item{webEnv}{}

}
}
