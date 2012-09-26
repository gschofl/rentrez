\name{elink-class}
\alias{[,elink-method}
\alias{[,elink,ANY,missing,ANY-method}
\alias{content,elink-method}
\alias{elink-class}
\title{\dQuote{elink} class}
\usage{
  .elink(...)

  \S4method{content}{elink}(x, ...)

  \S4method{[}{elink,ANY,missing,ANY}(x, i, j, ...,
    drop = TRUE)
}
\arguments{
  \item{...}{arguments passed to the constructor method}

  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{drop}{For matrices and arrays.  If \code{TRUE} the
  result is coerced to the lowest possible dimension (see
  the examples).  This only works for extracting elements,
  not for the replacement.  See \code{\link{drop}} for
  further details.  }
}
\description{
  \dQuote{elink} is an S4 class that provides a container
  for data retrived by calls to the NCBI ELink utility.
}
\section{Slots}{
  \describe{ \item{\code{url}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{error}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{content}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{id}:}{An
  \code{\linkS4class{idlist}} object.}
  \item{\code{databaseTo}:}{} \item{\code{command}:}{}
  \item{\code{queryKey}:}{} \item{\code{webEnv}:}{}
  \item{\code{linkset}:}{A list containing the linked
  data.} }
}

\section{Extends}{
  Class \code{\linkS4class{eutil}}, directly.
}
\seealso{
  \code{\link{elink}} for generating calls to the NCBI
  ELink utility.
}

