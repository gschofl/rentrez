% Built by roxygen3 0.1
\docType{class}
\name{elink-class}
\alias{elink}
\alias{elink-class}
\title{elink}
\usage{
  elink(id, dbFrom = NULL, dbTo = NULL, linkname = NULL,
    usehistory = FALSE, cmd = "neighbor",
    correspondence = FALSE, query_key = NULL,
    WebEnv = NULL, term = NULL, holding = NULL,
    datetype = NULL, reldate = NULL, mindate = NULL,
    maxdate = NULL)
}
\arguments{
  \item{id}{(Required) A character vector of UIDs.}

  \item{dbFrom}{Initial database containing the UIDs in the input list.}

  \item{dbTo}{Destination database from which to retrieve linked UIDs. If
not provided links will be sought in the database containing the input UIDs.}

  \item{linkname}{Name of the Entrez link to retrieve. Every link in
Entrez is given a name of the form 'dbFrom_dbTo_subset'.}

  \item{usehistory}{If \code{TRUE} search results are stored directly in
the user's Web environment so that they can be used in subsequents 
calls to \code{\link{esummary}} or \code{\link{efetch}}.}

  \item{cmd}{ELink command mode (default: 'neighbor'). See Details.}

  \item{correspondence}{if \code{TRUE} correspondence between query UIDs and
destination UIDs is preserved.}

  \item{query_key}{Query key.}

  \item{WebEnv}{Web Environment.}

  \item{term}{Search query to limit the output set of linked UIDs.}

  \item{holding}{Name of LinkOut provider.}

  \item{datetype}{Type of date to limit the search. One of 'mdat'
(modification date), 'pdat' (publication date) or 'edat' (Entrez date).}

  \item{reldate}{umber of days back for which search items are
returned.}

  \item{mindate}{Minimum date of search range. Format YYYY/MM/DD.}

  \item{maxdate}{Maximum date of search range. Format YYYY/MM/DD.}
}

\value{
An \code{elink} instance.
}
\description{
\dQuote{elink} is an S4 class that provides a container for data retrived
by calls to the NCBI ELink utility.

\code{elink} generates a list of UIDs in a specified Entrez database that
are linked to a set of input UIDs in either the same or another
database. For instance, the ELink utility can find Entrez gene records
linked to records in Entrez Protein.
}
\details{
See the official online documentation for NCBI's
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ELink}{EUtilities}
for additional information.

If \code{dbTo} and \code{dbFrom} are set to the same database, ELink will
return neighbors within that database.

Elink commands (cmd) specify the function that elink will perform.
Available commands are:
\itemize{
  \item{"\strong{neighbor}" }{(Default) ELink returns a set of UIDs in dbTo
  linked to the input UIDs in dbFrom.}
  \item{"\strong{neighbor_score}" }{ELink returns a set of UIDs within the
  same database as the input UIDs along with similarity scores.}
  \item{"\strong{neighbor_history}" }{ELink posts the output UIDs to the
  Entrez History server and returns a query_key and WebEnv parameter.
  Alternatively this is achieved by setting \code{usehistory = TRUE}}
  \item{"\strong{acheck}" }{ELink lists all links available for a set of UIDs.}
  \item{"\strong{ncheck}" }{ELink checks for the existence of links
  \emph{within the same database} for a set of UIDs.}
  \item{"\strong{lcheck}" }{Elink checks for the existence of external links
  (LinkOuts) for a set of UIDs.}
  \item{"\strong{llinks}" }{For each input UID, ELink lists the URLs and
  attributes for the LinkOut providers that are not libraries.}
  \item{"\strong{llinkslib}" }{For each input UID, ELink lists the URLs and
  attributes for all LinkOut providers including libraries.}
  \item{"\strong{prlinks}" }{ELink lists the primary LinkOut provider for
  each input UID.}
}
}
\section{Class hierarchy for elink}{
Super classes:
\itemize{
  \item \linkS4class{eutil}
}

}

\section{Generics with methods for elink}{
\itemize{
  \item \code{\link{[}}
  \item \code{\link{content}}
  \item \code{\link{count}}
  \item \code{\link{database}}
  \item \code{\link{idList}}
  \item \code{\link{linkSet}}
  \item \code{\link{queryKey}}
  \item \code{\link{show}}
  \item \code{\link{webEnv}}
}

}

\examples{
## Find related data through ELink #########################################

# Example: find a set of pubmed IDs linked to nucleotide GIs
gis <- c("84785887","84785899","84785907","84785905","84785889")
rv <- elink(gis, "nucleotide", "pubmed")
rv

# Fetch the XML records for the linked articles
efetch(rv)

# Example: Check whether two nucleotide sequences have any LinkOut providers.
gis <- c("84785887","84785899")
rv <- elink(gis, "nucleotide", cmd="lcheck")
rv

# Example: Find publication linked to a protein sequence
a <- esearch("Chlamydia and cpaf", "protein")
a
l <- elink(a[1], dbTo="pubmed")
l
efetch(l)
}

\keyword{classes}
\section{Slots}{
\describe{
  \item{url}{A character vector containing the query URL.}

  \item{error}{Any error or warning messages parsed from
the output of the call submitted to Entrez.}

  \item{content}{A \code{\linkS4class{raw}} vector holding the unparsed
contents of a request to Entrez.}

  \item{idList}{An object of class \linkS4class{idList}}

  \item{databaseTo}{An object of class \linkS4class{character}}

  \item{command}{An object of class \linkS4class{character}}

  \item{queryKey}{An object of class \linkS4class{integer}}

  \item{webEnv}{An object of class \linkS4class{character}}

  \item{linkSet}{A list containing the linked data.}

}
}
