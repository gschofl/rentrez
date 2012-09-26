\name{einfoDb-class}
\alias{content,einfoDb-method}
\alias{einfoDb-class}
\title{\dQuote{einfoDb class}}
\usage{
  .einfoDb(...)

  \S4method{content}{einfoDb}(x, ...)
}
\arguments{
  \item{...}{arguments passed to the constructor method}

  \item{x}{An \code{\linkS4class{eutil}} object.}
}
\value{
  A list
}
\description{
  \dQuote{einfoDb} is an S4 class that extends the
  \code{\link{einfo-class}}. This class provides a
  container for data retrived by calls to the
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI
  EInfo} utility.
}
\section{Slots}{
  \describe{ \item{\code{url}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{error}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{content}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{dbName}:}{Name
  of the target database} \item{\code{menuName}:}{Name of
  the target database} \item{\code{descriptiom}:}{Short
  description of the target database}
  \item{\code{records}:}{Count of records in the target
  database} \item{\code{lastUpdate}:}{Last update of the
  target database} \item{\code{fields}:}{Field names of the
  target database} \item{\code{links}:}{Available links for
  the target database} }
}

\section{Extends}{
  Class \code{"\linkS4class{einfo}"}, directly.  Class
  \code{"\linkS4class{eutil}"}, by class \dQuote{einfo}.
}
\seealso{
  \code{\link{einfo}} for generating calls to the NCBI
  EInfo utility.
}

