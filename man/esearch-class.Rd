\name{esearch-class}
\alias{[,esearch-method}
\alias{esearch-class}
\alias{esearch,esearch-method}
\alias{length,esearch-method}
\alias{show,esearch-method}
\title{esearch class}
\usage{
  .esearch(...)
}
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
  number of hits for a database query.} \item{retMax}{An
  integer giving the number of hits retrieved.}
  \item{retStart}{An integer giving the index of the first
  hit retrieved.} \item{queryKey}{Parameter specifying a
  UID list attached to a user's Web Environment on the
  Entrez history server.} \item{webEnv}{Parameter
  specifying the Web Environment on the Entrez history
  server containing the list of UIDs matching an Entrez
  query.} \item{queryTranslation}{A Character vector
  containing the search term as translated by the Entrez
  search system} \item{idList}{A Character vector
  containing the UIDs returned} }
}
\seealso{
  \code{\link{esearch}} for generating calls to the NCBI
  ESearch utility.
}
\keyword{internal}

