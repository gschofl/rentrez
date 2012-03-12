\name{esearch}
\alias{esearch}
\title{Search and retrieve primary UIDs matching a text query.}
\usage{
  esearch(term, db = "nucleotide", usehistory = FALSE,
    WebEnv = NULL, query_key = NULL, retstart = 0,
    retmax = 100, field = NULL, datetype = NULL,
    reldate = NULL, mindate = NULL, maxdate = NULL)
}
\arguments{
  \item{term}{A valid Entrez text query.}

  \item{db}{Database to search (default: nucleotide)}

  \item{usehistory}{If \code{TRUE} search results are
  stored drectly in the user's web environment so that they
  can be used in a subsequent call to \code{\link{efetch}},
  \code{\link{esummary}}, or \code{\link{elink}}. Also,
  usehistory must be set to \code{TRUE} for \code{esearch}
  to interpret query key values included in term or to
  accept a WebEnv as input.}

  \item{WebEnv}{Web environment string returned by a
  previous call to \code{\link{esearch}},
  \code{\link{epost}} or \code{\link{elink}}. When
  provided, \code{esearch} will append the results of the
  search to the pre-existing WebEn. Providing WebEnv also
  allows query keys to be used in term so that previous
  search sets can be combined or limited.}

  \item{query_key}{query key returned by a previous call to
  \code{\link{esearch}}, \code{\link{epost}} or
  \code{\link{elink}}. When provided, \code{esearch} will
  find the intersection of the set specified by query_key
  and the set retrieved by the query in term (i.e. joins
  the two with AND).}

  \item{retstart}{Numeric index of the first UID in the
  retrieved set to be shown in the XML output (default:
  0).}

  \item{retmax}{Total number of UIDs to be retrieved
  (default: 100).}

  \item{field}{Optional. Search field. Limits the entire
  search term, if used.}

  \item{datetype}{Optional. Type of date to limit the
  search. One of "mdat" (modification date), "pdat"
  (publication date) or "edat" (Entrez date)}

  \item{reldate}{Optional. Number of days back for which
  search items are returned.}

  \item{mindate}{Optional. Minimum date of search range.
  Format YYYY/MM/DD.}

  \item{maxdate}{Optional. Maximum date of search range.
  Format YYYY/MM/DD.}
}
\value{
  An \code{\link{esearch-class}} object.
}
\description{
  \code{esearch} searches and retrieves primary UIDs for
  use with \code{\link{efetch}}, \code{\link{esummary}},
  and \code{\link{elink}}, and optinally stores results in
  the user's web environment for future use.
}
\details{
  See the online documentation at
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}
  for additional information.
}
\examples{
# Search in Nucleotide for all tRNAs
  esearch(term = "biomol trna[prop]", db = "nucleotide")
}

