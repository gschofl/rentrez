\name{egquery-class}
\alias{egquery-class}
\title{\dQuote{egquery} class}
\usage{
  .equery(...)
}
\arguments{
  \item{...}{arguments passed to the constructor method}
}
\description{
  egquery is an S4 class that provides a container for data
  retrived by calls to the NCBI EGQuery utility.
}
\section{Slots}{
  \describe{ \item{\code{url}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{error}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{content}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{term}:}{The
  search term passed on to the EGQuery Utility}
  \item{\code{count}:}{A data frame with the following
  columns: \code{dbName}, \code{menuName}, \code{count},
  and \code{status}.} }
}

\section{Extends}{
  Class \code{"\linkS4class{eutil}"}, directly.
}
\seealso{
  \code{\link{egquery}} for generating calls to the NCBI
  EGQuery utility.
}

