\name{elink-class}
\alias{[,elink-method}
\alias{elink,elink-method}
\alias{elink-class}
\alias{show,elink-method}
\title{elink class}
\usage{
  .elink(...)
}
\description{
  elink is an S4 class that extends the
  \code{\link{eutil-class}}. This class provides a
  container for data retrived by calls to the NCBI ELink
  utility.
}
\details{
  elink objects have three slots in addition to the slots
  provided by basic \code{\link{eutil-class}} objects:
  \describe{ \item{databaseFrom}{The name of the database
  containing the input UIDs.} \item{databaseTo}{The name of
  the database from which UIDs were retrieved.}
  \item{command}{The ELink command mode used for a query.}
  \item{idList}{A character vector containing the UIDs
  sent.} \item{linkList}{A list of character vectors
  containing the UIDs returned.} }
}
\seealso{
  \code{\link{elink}} for generating calls to the NCBI
  ELink utility.
}
\keyword{internal}

