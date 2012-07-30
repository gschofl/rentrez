\name{esummary}
\alias{esummary}
\title{Retrieve document summaries (DocSums)}
\usage{
  esummary(id, db = NULL, query_key = NULL, WebEnv = NULL,
    retstart = 1, retmax = 10000, version = "default")
}
\arguments{
  \item{id}{(Required) List of UIDs provided either as a
  character vector, as an \code{\linkS4class{esearch}} or
  \code{\linkS4class{elink}} object, or by reference to a
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

  \item{retstart}{Numeric index of the first DocSum to be
  retrieved (default: 1).}

  \item{retmax}{Total number of DocSums from the input set
  to be retrieved (maximum: 10,000).}

  \item{version}{If "2.0" \code{esummary} will retrieve
  version 2.0 ESummary XML output.}
}
\value{
  An \code{\linkS4class{esummary}} object.
}
\description{
  \code{esummary} retrieves document summaries for a list
  of primary UIDs or for a set of UIDs stored in the user's
  web environment (using the Entrez History server).
}
\details{
  See the official online documentation for NCBI's
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary}{EUtilities}
  for additional information.
}
\examples{
## Downloading Document Summaries ##########################################

# Download DocSums for some protein GIs: 313848131,313847824,313847819,
# 313847818,313847817

id <- c(313848131,313847824,313847819,313847818,313847817)
doc <- esummary(id, "protein")
doc

# Download data from a previous ESearch.
(s <- esearch(term="evolution and ecology", db="journals", usehistory=TRUE))
doc <- esummary(s)

# Retrieve the contents of the esearch object as a data frame.
doc <- content(doc)
doc["Title"]

# Get accession numbers for a list of GIs
id <- c(313848131,313847824,313847819,313847818,313847817)
prot <- content(esummary(id, "protein"))
names(prot)
prot[,c("Id","Caption","Length","TaxId")]
}
\seealso{
  \code{\link{content}} to retrieve parsed DocSums from
  \code{\linkS4class{esummary}} objects.
}

