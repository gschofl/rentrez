% Built by roxygen3 0.1
\name{efetch.batch}
\alias{efetch.batch}
\title{Retrieve batches of data records in the requested format from NCBI}
\usage{
  efetch.batch(id, chunk_size = 200, rettype = NULL,
    retmode = NULL, retmax = NULL, strand = NULL,
    seq_start = NULL, seq_stop = NULL, complexity = NULL)
}
\arguments{
  \item{id}{(Required)
List of UIDs provided (via the Entrez History server) by an
\code{\linkS4class{esearch}}, \code{\linkS4class{epost}} or
\code{\linkS4class{elink}} object.}

  \item{chunk_size}{Number of records downloaded as a batch (default: 200;
maximum: 500).}

  \item{rettype}{A character string specifying the record view returned,
such as 'abstract' or 'medline' from PubMed, or 'gp' or 'fasta' from
protein.
See
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
for allowed values for each database.}

  \item{retmode}{A character string specifying the data format of the
records returned, such as plain text, XML, or asn.1.
See
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
for allowed values for each database.}

  \item{retmax}{Total number of records from the input set to be retrieved.}

  \item{strand}{Strand of DNA to retrieve. (1: plus strand, 2: minus strand)}

  \item{seq_start}{First sequence base to retrieve.}

  \item{seq_stop}{Last sequence base to retrieve.}

  \item{complexity}{Data content to return. (0: entire data structure,
1: bioseq, 2: minimal bioseq-set, 3: minimal nuc-prot, 4: minimal pub-set)}
}

\value{
An \code{\linkS4class{efetch}} object.
}
\description{
\code{efetch.batch} retrieves large data sets from NCBI in batches.
}
\details{
See the official online documentation for NCBI's
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}{EUtilities}
for additional information.
}
\examples{
## Download a large set of records (>500)

# Download all human ncRNAs in FASTA format
query <- "human[orgn] and biomol ncrna[prop]"

# Let's check how many there are
esearch(query, "nucleotide", rettype="count")

# upload the GIs to the history server
ncrna <- esearch(query, "nucleotide", usehistory=TRUE)
ncrna

## Not run
## download ncRNAs in batch mode
# fasta <- efetch.batch(id=ncrna, chunk_size=200, rettype="fasta")
# write(fasta, "~/human_small_nuclear_rna.fasta")
}

