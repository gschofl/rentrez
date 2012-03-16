\name{esummary}
\alias{esummary}
\title{Retrieve document summaries (DocSums)}
\usage{
  esummary(id, db = NULL, query_key = NULL, WebEnv = NULL,
    retstart = 1, retmax = 10000, version = "default")
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

  \item{retstart}{Numeric index of the first DocSum to be
  retrieved (default: 1).}

  \item{retmax}{Total number of DocSums from the input set
  to be retrieved (maximum: 10,000).}

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
# View Document Summaries from PubMed

doc <- esummary(id=c(22382892,22302240,22299031,22296995), db="pubmed")
doc

# View Document Summaries from Journals

doc <- esummary("37381", "journals")
doc

# Combine a esearch with esummary

doc <- esummary(esearch(term="Evolution", db="journals", retmax=10))
sapply(doc$documentSummary, "[", "Title")

# View Document Summaries from Gene

esummary(c(828392,790,470338), "gene")

# Get accession numbers for a list of GIs

prot <- esummary(c(403164,45447012,27806117), "protein")
prot
# use docsum() to transform the Document Summary into a data frame
accn <- docsum(prot)$caption
}

