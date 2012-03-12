\name{post-class}
\alias{epost,epost-method}
\alias{post-class}
\alias{show,epost-method}
\title{epost class}
\description{
  epost is an S4 class that extends the
  \code{\link{eutil-class}}. This class provides a
  container for data retrived by calls to the NCBI EPnfo
  utility.
}
\details{
  epost objects have three slots in addition to the slots
  provided by basic \code{\link{eutil-class}} objects:
  \describe{ \item{database}{The name of the queried
  database.} \item{queryKey}{query key parameter specifying
  the location on the Entrez history server of the list of
  UIDs matching the Entrez query.} \item{webEnv}{Web
  environment parameter specifying the location on the
  Entrez history server of the list of UIDs matching the
  Entrez query.} }
}
\seealso{
  \code{\link{epost}} for generating calls to the NCBI
  EPost utility.
}

