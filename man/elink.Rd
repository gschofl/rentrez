\name{elink}
\alias{elink}
\title{Retrive links to records in other Entrez databases}
\usage{
  elink(id, dbFrom = NULL, dbTo = NULL, usehistory = FALSE,
    cmd = "neighbor", correspondence = FALSE,
    query_key = NULL, WebEnv = NULL, linkname = NULL,
    term = NULL, holding = NULL, datetype = NULL,
    reldate = NULL, mindate = NULL, maxdate = NULL)
}
\arguments{
  \item{id}{(Required) A character vector of UIDs.}

  \item{dbFrom}{Initial database containing the UIDs in the
  input list.}

  \item{dbTo}{Target database where the linked records are
  sought.}

  \item{usehistory}{If \code{TRUE} search results are
  stored directly in the user's Web environment so that
  they can be used in subsequents call to
  \code{\link{esummary}} or \code{\link{efetch}}.}

  \item{cmd}{(default: 'neighbor')}

  \item{correspondence}{if \code{FALSE} all destination
  UIDs are lumped together, if \code{TRUE} correspondence
  between query UIDs and destination UIDs is preseverd.}

  \item{query_key}{Query key.}

  \item{WebEnv}{Web Environment.}

  \item{linkname}{}

  \item{term}{}

  \item{holding}{}

  \item{datetype}{}

  \item{reldate}{}

  \item{mindate}{}

  \item{maxdate}{}
}
\value{
  An \code{\link{elink-class}} object.
}
\description{
  \code{elink} generates a list of UIDs in a specified
  Entrez database that are linked to a set of input UIDs in
  either the same or another database. For instance, the
  ELink utility can find Entrez gene records linked to
  records in Entrez Protein.
}
\details{
  See the online documentation at
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ELink}
  for additional information.
}
\examples{
id_list <- c(194680922,50978626,28558982,9507199,6678417)
links <- elink(id=id_list, dbFrom="protein", dbTo="gene",
               cmd="neighbor_history")
links
}

