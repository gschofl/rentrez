\docType{package}
\name{rentrez-package}
\alias{rentrez-package}
\title{The rentrez package provides an interface to NCBI's
Entrez utilities.}
\description{
  NCBI provides the EUtilities web service for software to
  query the Entrez databases directly, rather than through
  the web interface. For more information see
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25500/}{here}.
}
\details{
  This package provides an interface to construct query
  URLs and parse query results.

  Important functions: \itemize{ \item \code{\link{einfo}}:
  Provides field names, term counts, last update, and
  available updates for each database.

  \item \code{\link{esearch}}: Searches and retrieves
  primary UIDs (for use with \code{epost}, \code{esummary},
  \code{elink}, or \code{efetch})

  \item \code{\link{epost}}: Uploads primary UIDs to the
  Entrez history server for subsequent use with e.g.
  \code{esummary}, \code{elink}, or \code{efetch})

  \item \code{\link{esummary}}: Retrieves document
  summaries (DocSums) from a list of primary UIDs.

  \item \code{\link{elink}}: Returns a list of UIDs (and
  relevancy scores) from a target database that are related
  to a list of UIDs in the same database or in another
  Entrez database.

  \item \code{\link{efetch}}: Retrieves data records in a
  specified format corresponding to a list of UIDs. }
}
\examples{
############################################################################
# combine esearch with esummary/efetch

# Download PubMed records that are indexed in MeSH for both 'Clamydia' and 
# 'genome' and were published in 2011.
query <- "Chlamydia[mesh] and genome[mesh] and 2011[pdat]"

# check how many records fit the bill and download the PMIDs
n <- ecount(query, db="pubmed")
pmids <- esearch(query, db="pubmed", retmax=n)

# Have a quick look at the titles
# doc <- esummary(pmids)

# Fetch the abstracts and write them to file
abstracts <- efetch(pmids, rettype="abstract")
write(abstracts, file="~/chlamydia_genome_2011.pmd")

############################################################################
# combine epost with esummary/efetch

# Download protein records corresponding to a list of GI numbers.
id_list <- c(194680922,50978626,28558982,9507199,6678417)

# post the GI numbers to the Entrez history server
p <- epost(id_list, "protein")

# retrieve docsums with esummary
# docsums <- docsum(esummary(p))
# docsums

# get FASTAs with efetch and write them to file
prot <- efetch(p, rettype="fasta")
write(prot, file="~/prot.fa")
}
\author{
  Gerhard \enc{Schöfl}{Schofl} \email{gschofl@yahoo.de}
}
\keyword{package}

