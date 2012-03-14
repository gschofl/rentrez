\name{esummary}
\alias{esummary}
\title{Retrieve document summaries (DocSums)}
\usage{
  esummary(id, db = NULL, query_key = NULL, WebEnv = NULL,
    version = "default")
}
\arguments{
  \item{id}{(Required) List of UIDs provided either as a
  character vector, as an \code{\link{esearch-class}} or
  \code{\link{elink-class}} object, or by reference to a
  Web Environment and a query key obtained directly from
  objects returned by previous calls to
  \code{\link{esearch}}, \code{\link{epost}} or
  \code{\link{elink}}. If UIDs are provided as a plain
  character vector, \code{db} must be specified explicitly,
  and all of the UIDs must be from the database specified
  by \code{db}.}

  \item{db}{(Required only when \code{id} is a character
  vector of UIDs) Database from which to retrieve DocSums.}

  \item{query_key}{An integer specifying which of the UID
  lists attached to a user's Web Environment will be used
  as input to \code{efetch}. (Usually obtained drectely
  from objects returned by previous \code{\link{esearch}},
  \code{\link{epost}} or \code{\link{elink}} calls.)}

  \item{WebEnv}{A character string specifying the Web
  Environment that contains the UID list. (Usually obtained
  directely from objects returned by previous
  \code{\link{esearch}}, \code{\link{epost}} or
  \code{\link{elink}} calls.)}

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
  web environment (using the Entrez History server).
}
\details{
  See the online documentation for additional information
  (\url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary})
}
\examples{
##
}

