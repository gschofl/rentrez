% Built by roxygen3 0.1
\docType{class}
\name{esearch-class}
\alias{esearch}
\alias{esearch-class}
\title{esearch}
\usage{
  esearch(term, db = "nuccore", rettype = "uilist",
    retstart = 0, retmax = 100, usehistory = FALSE,
    WebEnv = NULL, query_key = NULL, field = NULL,
    datetype = NULL, reldate = NULL, mindate = NULL,
    maxdate = NULL)
}
\arguments{
  \item{term}{A valid Entrez text query.}

  \item{db}{Database to search (default: nuccore).}

  \item{rettype}{Retrieval type. (default: 'uilist', alternative: 'count'.)}

  \item{retstart}{Numeric index of the first UID in the
retrieved set to be shown in the XML output (default: 0).}

  \item{retmax}{Total number of UIDs to be retrieved (default: 100).}

  \item{usehistory}{If \code{TRUE} search results are stored directly in
the user's Web environment so that they can be used in a subsequent 
call to \code{\link{esummary}}, \code{\link{efetch}}, or
\code{\link{elink}}. Also, \code{usehistory} must be set to \code{TRUE}
for \code{esearch} to interpret query key values included in \code{term}
or to accept a \code{WebEnv} as input.}

  \item{WebEnv}{Web environment string returned by a previous call to
\code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
When provided, \code{esearch} will append the results of the search to
the pre-existing WebEnv. Providing WebEnv also allows query keys to be
used in \code{term} so that previous search sets can be combined or
limited.}

  \item{query_key}{query key returned by a previous call to
\code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
When provided, \code{esearch} will find the intersection of the set
specified by query_key and the set retrieved by the query in \code{term}
(i.e. joins the two with AND).}

  \item{field}{Optional. Search field used to limit the entire search
term.}

  \item{datetype}{Optional. Type of date to limit the search. One of "mdat"
(modification date), "pdat" (publication date) or "edat" (Entrez date)}

  \item{reldate}{Optional. Number of days back for which search items are
returned.}

  \item{mindate}{Optional. Minimum date of search range. Format
YYYY/MM/DD, YYYY/MM, or YYYY.}

  \item{maxdate}{Optional. Maximum date of search range. Format
YYYY/MM/DD, YYYY/MM, or YYYY.}
}

\value{
An \code{esearch} instance.
}
\description{
\dQuote{esearch} is an S4 class that provides a container for data
retrived by calls to the NCBI ESearch utility.

\code{esearch} searches and retrieves primary UIDs matching a text query
for use with \code{\link{efetch}}, \code{\link{esummary}}, and
\code{\link{elink}}. \code{esearch} can post its output set of UIDs to the
Entrez history server if the \code{usehistory} parameter is set \code{TRUE}.
The resulting \code{esearch} object can be passed on to
\code{\link{esummary}}, \code{\link{efetch}}, or \code{\link{elink}}.
}
\details{
See the official online documentation for NCBI's
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch}{EUtilities}
for additional information.
}
\section{Class hierarchy for esearch}{
Super classes:
\itemize{
  \item \linkS4class{eutil}
}

}

\section{Generics with methods for esearch}{
\itemize{
  \item \code{\link{[}}
  \item \code{\link{content}}
  \item \code{\link{count}}
  \item \code{\link{database}}
  \item \code{\link{idList}}
  \item \code{\link{length}}
  \item \code{\link{queryKey}}
  \item \code{\link{queryTranslation}}
  \item \code{\link{retmax}}
  \item \code{\link{retstart}}
  \item \code{\link{show}}
  \item \code{\link{webEnv}}
}

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
# To ensure that we retrieve the full list of IDs, we count
# the number of hits to set the 'retmax' parameter.

query <- "BRCA1 and human"
n <- count(esearch(query, "protein", rettype="count"))
brca1 <- esearch(query, "protein", retmax=n)

## Storing search results on the Entrez history server #####################

query <- "BRCA1 and human"
brca1 <- esearch(query, "protein", retmax=2443, usehistory=TRUE)
brca1
}

\seealso{
\code{\link{esummary}}, \code{\link{efetch}}, \code{\link{elink}}
}
\keyword{classes}
\section{Slots}{
\describe{
  \item{url}{A character vector containing the query URL.}

  \item{error}{Any error or warning messages parsed from
the output of the call submitted to Entrez.}

  \item{content}{A character vector holding the unparsed
contents of a request to Entrez.}

  \item{idList}{An object of class \linkS4class{idList}}

}
}
