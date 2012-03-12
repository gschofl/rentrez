\name{esummary}
\alias{esummary}
\title{Retrieve document summaries (DocSums)}
\usage{
  esummary(id, db = attr(id, "database"), query_key = NULL,
    WebEnv = NULL, version = "default")
}
\arguments{
  \item{id}{(Required unless input is from the Entrez
  History server.) List of UIDs provided either as a
  character vector or as an \code{\link{esearch-class}}
  object. If UIDs are provided as a plain character vector
  the database from which to retrieve DocSums must be
  specified explicitly.}

  \item{db}{(Required unless UIDs are provided as an
  \code{\link{esearch-class}} object.) Database from which
  to retrieve DocSums.}

  \item{query_key}{An integer specifying which of the UID
  lists attached to a user's Web Environment will be used
  as input to \code{esummary}. Query keys are obtained from
  the output of previous \code{\link{esearch}},
  \code{\link{epost}} or \code{\link{elink}} calls.}

  \item{WebEnv}{A character string specifying the Web
  Environment that contains the UID list to be provided as
  input to \code{esummary}. The WebEnv value is obtained
  from the output of previous \code{\link{esearch}},
  \code{\link{epost}} or \code{\link{elink}} calls.}

  \item{version}{If "2.0" \code{esummary} will retrieve
  version 2.0 ESummary XML output but not, currently,
  attempt to parse it.}
}
\value{
  An \code{\link{esummary-class}} object.
}
\description{
  \code{esummary} retrieves document summaries for a list
  of primary UIDs or for a set of UIDs stored in the user's
  web environment (using the Entrez History server). See
  the online documentation for additional information
  (\url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary})
}
\examples{
##
}

