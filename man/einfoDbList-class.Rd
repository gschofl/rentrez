\name{einfoDbList-class}
\alias{[,einfoDbList-method}
\alias{einfoDbList-class}
\title{einfoDbList class}
\description{
  einfoDbList is an S4 class that extends the
  \code{\link{einfo-class}}. This class provides a
  container for data retrived by calls to the NCBI EInfo
  utility.
}
\details{
  einfoDbList objects have one slots in addition to the
  slots provided by basic \code{\link{eutil-class}}
  objects: \describe{ \item{dbList}{A list of the names of
  all valid Entrez databases} }
}
\seealso{
  \code{\link{einfo}} for generating calls to the NCBI
  EInfo utility.
}

