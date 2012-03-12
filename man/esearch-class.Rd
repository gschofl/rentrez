\name{esearch-class}
\alias{[,esearch-method}
\alias{esearch-class}
\alias{esearch,esearch-method}
\alias{length,esearch-method}
\alias{show,esearch-method}
\title{esearch class}
\description{
  esearch is an S4 class that extends the
  \code{\link{eutil-class}}. This class provides a
  container for data retrived by calls to the NCBI ESearch
  utility.
}
\details{
  esearch objects have eight slots in addition to the slots
  provided by basic \code{\link{eutil-class}} objects:
  \describe{ \item{database}{The name of the queried
  database.} \item{count}{An integer giving the total
  number of hits for a database query} \item{retMax}{An
  integer giving the number of hits retrieved}
  \item{retStart}{An integer giving the index of the first
  hit retrieved} \item{queryKey}{query key parameter
  specifying the location on the Entrez history server of
  the list of UIDs matching the Entrez query.}
  \item{webEnv}{Web environment parameter specifying the
  location on the Entrez history server of the list of UIDs
  matching the Entrez query.}
  \item{queryTranslation}{character vector containing the
  search term as translated by the Entrez search system}
  \item{idList}{character vector containing the UIDs
  returned} }
}
\seealso{
  \code{\link{esearch}} for generating calls to the NCBI
  ESearch utility.
}

