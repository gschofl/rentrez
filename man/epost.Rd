\name{epost}
\alias{epost}
\title{Post a list of primary UIDs to the Entrez history server}
\usage{
  epost(id, db = NULL, WebEnv = NULL)
}
\arguments{
  \item{id}{(Required) List of UIDs.}

  \item{db}{(Required) Database containing the UIDs in the
  input list.}

  \item{WebEnv}{(Optional) Web Environment. If provided,
  this parameter specifies the Web Environment that will
  receive the UIDs sent by \code{epost}. EPost will create
  a new query key associated with that Web Environment. The
  WebEnv value is usually returned by a previous call to
  \code{\link{esearch}}, \code{\link{epost}} or
  \code{\link{elink}}. If no WebEnv parameter is provided,
  EPost will create a new Web Environment and post the UIDs
  to query_key 1.}
}
\value{
  An \code{\linkS4class{epost}} object.
}
\description{
  The EPost utility allows a list of UIDs to be uploaded to
  the Entrez History Server and returns an integer label
  called a query key and an encoded cookie string called a
  Web environment. \code{\linkS4class{epost}} objects can
  then be used in stead of a UID list in subsequent
  \code{\link{esummary}}, \code{\link{efetch}}, or
  \code{\link{elink}} calls.
}
\details{
  See the official online documentation for NCBI's
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EPost}{EUtilities}
  for additional information.
}
\examples{
## Uploading UIDs to the Entrez history server #############################

id <- c(194680922,50978626,28558982,9507199,6678417)
p <- epost(id, "protein")
p
}

