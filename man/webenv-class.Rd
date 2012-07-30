\name{webenv-class}
\alias{show,webenv-method}
\alias{webenv-class}
\title{\dQuote{webenv} class}
\usage{
  .webenv(...)
}
\description{
  A container for web environments
}
\section{Slots}{
  \describe{ \item{\code{database}:}{Database from which
  the UIDs were retrieved} \item{\code{retStart}:}{The
  index of the first UID that is returned by an
  \code{esearch} call.} \item{\code{retMax}:}{The number of
  UIDs out of the total number of records that is returned
  by an \code{esearch} call.} \item{\code{count}:}{The
  total number of records matching a query.}
  \item{\code{queryTranslation}:}{The search term as
  translated by the Entrez search system}
  \item{\code{queryKey}:}{Integer query key returned by an
  esearch call when \strong{usehistory} is set \code{TRUE}}
  \item{\code{webEnv}:}{Web environment string returned
  from an esearch call when \strong{usehistory} is set
  \code{TRUE}} }
}
\keyword{internal}

