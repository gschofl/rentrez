\name{esearch}
\alias{esearch}
\title{Search and retrieve primary UIDs matching a text query}
\usage{
  esearch(term, db = "nuccore", usehistory = FALSE,
    WebEnv = NULL, query_key = NULL, retstart = 0,
    retmax = 100, rettype = "uilist", field = NULL,
    datetype = NULL, reldate = NULL, mindate = NULL,
    maxdate = NULL)
}
\arguments{
  \item{term}{A valid Entrez text query.}

  \item{db}{Database to search (default: nucleotide).}

  \item{usehistory}{If \code{TRUE} search results are
  stored directly in the user's Web environment so that
  they can be used in a subsequent call to
  \code{\link{esummary}}, \code{\link{efetch}}, or
  \code{\link{elink}}. Also, \code{usehistory} must be set
  to \code{TRUE} for \code{esearch} to interpret query key
  values included in \code{term} or to accept a
  \code{WebEnv} as input.}

  \item{WebEnv}{Web environment string returned by a
  previous call to \code{\link{esearch}},
  \code{\link{epost}} or \code{\link{elink}}. When
  provided, \code{esearch} will append the results of the
  search to the pre-existing WebEnv. Providing WebEnv also
  allows query keys to be used in \code{term} so that
  previous search sets can be combined or limited.}

  \item{query_key}{query key returned by a previous call to
  \code{\link{esearch}}, \code{\link{epost}} or
  \code{\link{elink}}. When provided, \code{esearch} will
  find the intersection of the set specified by query_key
  and the set retrieved by the query in \code{term} (i.e.
  joins the two with AND).}

  \item{retstart}{Numeric index of the first UID in the
  retrieved set to be shown in the XML output (default:
  0).}

  \item{retmax}{Total number of UIDs to be retrieved
  (default: 100).}

  \item{rettype}{Retrieval type. (default: 'uilist',
  alternative: 'count'.)}

  \item{field}{Optional. Search field used to limit the
  entire search term.}

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
  The ESearch utility searches and retrieves primary UIDs
  for use with \code{\link{efetch}},
  \code{\link{esummary}}, and \code{\link{elink}}.
  \code{esearch} can also post its output set of UIDs to
  the Entrez history server if the \code{usehistory}
  parameter is set to \code{TRUE}. The resulting
  \code{\link{esearch-class}} object from either can then
  be passed on in place of a UID list
  \code{\link{esummary}}, \code{\link{efetch}}, or
  \code{\link{elink}}.
}
\details{
  See the official online documentation for NCBI's
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}{EUtilities}
  for additional information.
}
\examples{
## Basic searching #########################################################

# Get th PubMed IDs (PMIDs) for articles about Chlamydia psittaci
# that have been published in 2011.

cpsit <- esearch(term="Chlamydia psittaci and 2011[pdat]", db="pubmed")
cpsit

# Search in PubMed for articles published by the journal PNAS in Volume 97.

pnas <- esearch(term="PNAS[jour] and 97[vol]", db="pubmed")
pnas

# Search for protein UIDs corresponding to BRCA1 in humans.
# To ensure that we retrieve the full list of IDs, we use 
# ecount() to set the 'retmax' parameter.

query <- "BRCA1 and human"
brca1 <- esearch(query, "protein", retmax=ecount(query, "protein"))

## Storing search results on the Entrez history server #####################

query <- "BRCA1 and human"
brca1 <- esearch(query, "protein", retmax=2443, usehistory=TRUE)
brca1
}

