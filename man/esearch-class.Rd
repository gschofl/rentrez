\name{esearch-class}
\alias{[,esearch,numeric,missing-method}
\alias{[,esearch-method}
\alias{[,esummary,ANY,ANY-method}
\alias{content,esearch-method}
\alias{esearch-class}
\alias{length,esearch-method}
\title{\dQuote{esearch} class}
\usage{
  .esearch(...)

  \S4method{content}{esearch}(x, ...)

  \S4method{[}{esearch,numeric,missing}(x, i, j, ...,
    drop = TRUE)

  \S4method{length}{esearch}(x)

  \S4method{[}{esummary,ANY,ANY}(x, i, j, ..., drop = TRUE)
}
\arguments{
  \item{...}{arguments passed to the constructor method}

  \item{x}{an \code{\link{eutil-class}} object.}

  \item{drop}{For matrices and arrays.  If \code{TRUE} the
  result is coerced to the lowest possible dimension (see
  the examples).  This only works for extracting elements,
  not for the replacement.  See \code{\link{drop}} for
  further details.  }
}
\value{
  an \code{\linkS4class{idlist}} object.
}
\description{
  esearch is an S4 class that provides a container for data
  retrived by calls to the NCBI ESearch utility.
}
\section{Slots}{
  \describe{ \item{\code{id}:}{An
  \code{\linkS4class{idlist}} or
  \code{\linkS4class{webenv}} object.}
  \item{\code{url}:}{See \code{\linkS4class{eutil}}.}
  \item{\code{error}:}{See \code{\linkS4class{eutil}}.}
  \item{\code{content}:}{See \code{\linkS4class{eutil}}.} }
}

\section{Extends}{
  Class \code{"\linkS4class{eutil}"}, directly.
}
\seealso{
  \code{\link{esearch}} for generating calls to the NCBI
  ESearch utility.
}

