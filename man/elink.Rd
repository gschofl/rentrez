\name{elink}
\alias{elink}
\title{Post a list of primary UIDs to the NCBI history server.}
\usage{
  elink(id, dbFrom = attr(id, "database"),
    dbTo = attr(id, "database"), cmd = "neighbor",
    query_key = NULL, WebEnv = NULL, linkname = NULL,
    term = NULL, holding = NULL, datetype = NULL,
    reldate = NULL, mindate = NULL, maxdate = NULL)
}
\arguments{
  \item{id}{A character vector of UIDs.}

  \item{db}{Database containing the UIDs in the input
  list.}

  \item{WebEnv}{Web Environment. If provided, this
  parameter specifies the Web Environment that will receive
  the UIDs sent by \code{epost}. EPost will create a new
  query key associated with that Web Environment. The
  WebEnv value is usually returned by a previous call to
  \code{\link{esearch}}, \code{\link{epost}} or
  \code{\link{elink}}. If no WebEnv parameter is provided,
  EPost will create a new Web Environment and post the UIDs
  to query_key 1.}
}
\value{
  An \code{\link{epost-class}} object.
}
\description{
  \code{epost} posts a list of UIDs for future use the the
  user's web environment for access with
  \code{\link{esummary}} or \code{\link{efetch}}.
}
\details{
  See the online documentation at
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EPost}
  for additional information.
}

