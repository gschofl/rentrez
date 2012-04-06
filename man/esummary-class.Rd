\name{esummary-class}
\alias{[,esummary-method}
\alias{$,esummary-method}
\alias{docsum,esummary-method}
\alias{esummary-class}
\alias{esummary,esummary-method}
\alias{length,esummary-method}
\alias{show,esummary-method}
\title{esummary class}
\usage{
  .esummary(...)
}
\arguments{
  \item{...}{arguments passed to the constructor method}
}
\description{
  esummary is an S4 class that extends the
  \code{\link{eutil-class}}. This class provides a
  container for data retrived by calls to the NCBI ESummary
  utility.
}
\details{
  esummary objects have two slots in addition to the slots
  provided by basic \code{\link{eutil-class}} objects:
  \describe{ \item{database}{The name of the queried
  database} \item{documentSummary}{A named list holding the
  parsed contents of the XML DocSums returned from a call
  to the NCBI ESearch utility.} }
}
\seealso{
  \code{\link{esummary}} for generating calls to the NCBI
  ESummary utility.
}
\keyword{internal}

