\name{egquery-class}
\alias{egquery,egquery-method}
\alias{egquery-class}
\alias{show,egquery-method}
\title{egquery class}
\description{
  egquery is an S4 class that extends the
  \code{\link{eutil-class}}. This class provides a
  container for data retrived by calls to the NCBI EGQuery
  utility.
}
\details{
  eqquery objects have two slots in addition to the slots
  provided by basic \code{\link{eutil-class}} objects:
  \describe{ \item{term}{The search term passed on to the
  EGQuery Utility} \item{count}{A data frame with the
  following columns: \code{dbName}, \code{menuName},
  \code{count}, and \code{status}.} }
}
\seealso{
  \code{\link{egquery}} for generating calls to the NCBI
  ELink utility.
}

