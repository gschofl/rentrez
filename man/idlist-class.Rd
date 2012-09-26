\name{idlist-class}
\alias{[,idlist-method}
\alias{[,idlist,numeric,missing,ANY-method}
\alias{idlist-class}
\alias{length,idlist-method}
\alias{show,idlist-method}
\title{\dQuote{idlist} class}
\usage{
  .idlist(...)

  \S4method{[}{idlist,numeric,missing,ANY}(x, i, j, ...,
    drop = TRUE)

  \S4method{length}{idlist}(x)
}
\arguments{
  \item{drop}{For matrices and arrays.  If \code{TRUE} the
  result is coerced to the lowest possible dimension (see
  the examples).  This only works for extracting elements,
  not for the replacement.  See \code{\link{drop}} for
  further details.  }

  \item{x}{an \R object.  For replacement, a vector or
  factor.}
}
\description{
  A container for UIDs
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
  \item{\code{uid}:}{A character vector holding the
  retrieved UIDs.} }
}
\keyword{internal}

