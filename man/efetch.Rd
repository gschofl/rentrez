% Built by roxygen3 0.1
\docType{class}
\name{efetch-class}
\alias{efetch}
\alias{efetch-class}
\title{efetch}
\usage{
  efetch(id, db = NULL, query_key = NULL, WebEnv = NULL,
    rettype = NULL, retmode = NULL, retstart = NULL,
    retmax = 500, strand = NULL, seq_start = NULL,
    seq_stop = NULL, complexity = NULL)
}
\arguments{
  \item{id}{(Required)
List of UIDs provided either as a character vector, as an
\code{\linkS4class{esearch}} instance, or by reference to a web environment
and a query key obtained directly from previous calls to
\code{\linkS4class{esearch}} (if \code{usehistory} was set \code{TRUE}),
\code{\linkS4class{epost}} or \code{\linkS4class{elink}}.
If UIDs are provided as a plain character vector, \code{db} must be
specified explicitly, and all of the UIDs must be from the database
specified by \code{db}.}

  \item{db}{(Required only when \code{id} is a character vector of UIDs)
Database from which to retrieve records. See
\href{http://www.ncbi.nlm.nih.gov/books/NBK25497/table/chapter2.chapter2_table1/?report=objectonly}{here}
for the supported databases.}

  \item{query_key}{An integer specifying which of the UID lists attached
to a user's Web Environment will be used as input to \code{efetch}.
(Usually obtained drectely from objects returned by previous
\code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
\code{\linkS4class{elink}} calls.)}

  \item{WebEnv}{A character string specifying the Web Environment that
contains the UID list. (Usually obtained directely from objects returned
by previous \code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
\code{\linkS4class{elink}} calls.)}

  \item{rettype}{A character string specifying the report type returned,
such as 'abstract' or 'medline' from PubMed, 'gp' or 'fasta' from
protein, or 'gb', 'gbwithparts, or 'fasta_cds_na' from nuccore.
See
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
for allowed values for each database.}

  \item{retmode}{A character string specifying the data mode of the
records returned, such as plain text, XML, or asn.1. See 
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
for allowed values for each database.}

  \item{retstart}{Numeric index of the first record to be retrieved.}

  \item{retmax}{Total number of records from the input set to be retrieved.}

  \item{strand}{Strand of DNA to retrieve. (1: plus strand, 2: minus strand)}

  \item{seq_start}{First sequence base to retrieve.}

  \item{seq_stop}{Last sequence base to retrieve.}

  \item{complexity}{Data content to return. (0: entire data structure,
1: bioseq, 2: minimal bioseq-set, 3: minimal nuc-prot, 4: minimal pub-set)}
}

\value{
An \code{efetch} instance.
}
\description{
\dQuote{efetch} is an S4 class that provides a container for data retrived
by calls to the NCBI EFetch utility.

\code{efetch} retrieves data records in the requested format from a
character vector of one or more primary UIDs or from a set of UIDs stored 
in the user's web environment.
}
\details{
See the official online documentation for NCBI's
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}{EUtilities}
for additional information.

The default retrieval mode (\code{retmode}) for the \code{pubmed},
\code{nuccore}, \code{protein}, and \code{gene} databases is 'text'. Default
\code{rettype}s are 'medline', 'gb', 'gp', and 'gene_table', respectively.
}
\section{Class hierarchy for efetch}{
Super classes:
\itemize{
  \item \linkS4class{eutil}
}

}

\section{Generics with methods for efetch}{
\itemize{
  \item \code{\link{c}}
  \item \code{\link{content}}
  \item \code{\link{database}}
  \item \code{\link{retmode}}
  \item \code{\link{rettype}}
  \item \code{\link{show}}
  \item \code{\link{write}}
}

}

\examples{
# Search the protein database for Chlamydia CPAF:
cpaf <- esearch("Chlamydia[organism] and CPAF", "protein")
cpaf

# Fetch the fasta sequence of the first 5 hits:
cpaf_fasta <- efetch(cpaf[1:5], rettype="fasta")
cpaf_fasta

# Directly download sequences using GIs:
gis <- c("84785889","84785885")
a <- efetch(gis, "nucleotide", rettype="fasta")

# Retrieve the downloaded record as text string:
seq <- content(a)

# Alternatively use accession numbers:
acc_no <- "AAA23146"
b <- efetch(acc_no, "protein", rettype="fasta")

# Download nucleotide GIs 84785889 and 84785885 in GenBank format (default):
gis <- c("84785889","84785885")
c <- efetch(gis, "nucleotide")

# Write to file
write(c, file="~/data.gbk")

# Download data from pubmed
query <- "Chlamydia psittaci and genome and 2012[pdat]"
cpsit <- esearch(query, "pubmed", usehistory=TRUE)
publ <- efetch(cpsit)

# retrieve the xml data
publ_xml <- content(publ)
}

\seealso{
\code{\link{efetch.batch}} for downloading more than about 500
data records.
\code{\link{content}} to retrieve data from \code{\linkS4class{efetch}}
objects.
}
\keyword{classes}
\section{Slots}{
\describe{
  \item{url}{A character vector containing the query URL.}

  \item{error}{Any error or warning messages parsed from
the output of the call submitted to Entrez.}

  \item{content}{A \code{\linkS4class{raw}} vector holding the unparsed
contents of a request to Entrez.}

  \item{database}{A character vector giving the name of the queried database.}

  \item{rettype}{Retrieval Mode. A character vector specifying the record
view returned, such as \sQuote{Abstract} or \sQuote{MEDLINE} from
\emph{pubmed}, or \sQuote{GenPept} or \sQuote{FASTA} from \emph{protein}.}

  \item{retmode}{Retrieval Mode. A character vector specifying the data format
of the records returned, such as plain \sQuote{text}, \sQuote{HMTL} or 
\sQuote{XML}.}

}
}
