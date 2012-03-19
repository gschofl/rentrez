\name{elink}
\alias{elink}
\title{Retrieve links to records in other Entrez databases}
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

  \item{dbTo}{Destination database where the linked records
  are sought.}

  \item{usehistory}{If \code{TRUE} search results are
  stored directly in the user's Web environment so that
  they can be used in subsequents calls to
  \code{\link{esummary}} or \code{\link{efetch}}.}

  \item{cmd}{ELink command mode (default: 'neighbor').}

  \item{correspondence}{if \code{FALSE} all destination
  UIDs are lumped together, if \code{TRUE} correspondence
  between query UIDs and destination UIDs is preserved.}

  \item{query_key}{Query key.}

  \item{WebEnv}{Web Environment.}

  \item{linkname}{Name of the Entrez link to retrieve.
  Every link in Entrez is given a name of the form
  'dbFrom_dbTo_subset'.}

  \item{term}{Seqrch query to limit the output set of
  linked UIDs.}

  \item{holding}{Name of LinkOut provider.}

  \item{datetype}{Type of date to limit the search. One of
  'mdat' (modification date), 'pdat' (publication date) or
  'edat' (Entrez date).}

  \item{reldate}{umber of days back for which search items
  are returned.}

  \item{mindate}{Minimum date of search range. Format
  YYYY/MM/DD.}

  \item{maxdate}{Maximum date of search range. Format
  YYYY/MM/DD.}
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
  See the official online documentation for NCBI's
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ELink}{EUtilities}
  for additional information.

  If \code{dbTo} and \code{dbFrom} are set to the same
  database, ELink will return neighbors within that
  database.
}
\examples{
## Find related data through ELink #########################################

# find a set of pubmed IDs linked to nucleotide GIs
gi_list <- c("84785887","84785899","84785907","84785905","84785889")
links <- elink(id=gi_list, dbFrom="nucleotide", dbTo="pubmed")
links

esummary(links)
}

