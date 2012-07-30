\name{esummary-class}
\alias{content,esummary-method}
\alias{esummary-class}
\title{\dQuote{esummary} class}
\usage{
  .esummary(...)

  \S4method{content}{esummary}(x, ...)
}
\arguments{
  \item{...}{arguments passed to the constructor method}

  \item{x}{an \code{\link{eutil-class}} object.}
}
\description{
  esummary is an S4 class that provides a container for
  data retrived by calls to the NCBI ESummary utility.
}
\section{Slots}{
  \describe{ \item{\code{url}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{error}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{content}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{database}:}{The
  name of the queried database.} \item{\code{version}:}{The
  version of the document summary requested.}
  \item{\code{docsum}:}{The parsed document summaries for a
  list of input UIDs} }
}

\section{Extends}{
  Class \code{"\linkS4class{eutil}"}, directly.
}
\seealso{
  \code{\link{esummary}} for generating calls to the NCBI
  ESummary utility.
}

