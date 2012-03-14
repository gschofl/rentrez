\name{elink-class}
\alias{elink-class}
\alias{elink,elink-method}
\alias{show,elink-method}
\title{elink class}
\description{
  elink is an S4 class that extends the
  \code{\link{eutil-class}}. This class provides a
  container for data retrived by calls to the NCBI ELink
  utility.
}
\details{
  epost objects have three slots in addition to the slots
  provided by basic \code{\link{eutil-class}} objects:
  \describe{ \item{databaseFrom}{The name of the database
  containing the input UIDs.} \item{databaseTo}{The name of
  the database from which UIDs were retrieved.}
  \item{command}{The ELink command mode used for a query.}
  \item{idList}{A character vector containing the UIDs
  sent.} \item{linkList}{A character vector containing the
  UIDs returned.} }
}
\seealso{
  \code{\link{elink}} for generating calls to the NCBI
  ELink utility.
}

