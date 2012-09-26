\name{einfoDbList-class}
\alias{[,einfoDb-method}
\alias{[,einfoDbList,numeric,missing,ANY-method}
\alias{content,einfoDbList-method}
\alias{einfoDbList-class}
\title{\dQuote{einfoDbList} class}
\usage{
  .einfoDbList(...)

  \S4method{content}{einfoDbList}(x, ...)

  \S4method{[}{einfoDbList,numeric,missing,ANY}(x, i, j,
    ..., drop = TRUE)
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
  \dQuote{einfoDbList} is an S4 class that provides a
  container for data retrived by calls to the
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI
  EInfo} utility.
}
\section{Slots}{
  \describe{ \item{\code{url}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{error}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{content}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{dbList}:}{A list
  of the names of all valid Entrez databases.} }
}

\section{Extends}{
  Class \code{"\linkS4class{einfo}"}, directly.  Class
  \code{"\linkS4class{eutil}"}, by class \dQuote{einfo}.
}
\seealso{
  \code{\link{einfo}} for generating calls to the NCBI
  EInfo utility.
}

