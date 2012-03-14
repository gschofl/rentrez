\name{efetch.batch}
\alias{efetch.batch}
\title{Retrieve batches of data records in the requested format from NCBI}
\usage{
  efetch.batch(id, chunk_size = 200, rettype = NULL,
    retmode = NULL, strand = NULL, seq_start = NULL,
    seq_stop = NULL, complexity = NULL)
}
\arguments{
  \item{id}{(Required) List of UIDs provided (via the
  Entrez History server) by an \code{\link{esearch-class}},
  \code{\link{epost-class}} or \code{\link{elink-class}}
  object.}

  \item{chunk_size}{Number of records downloaded as a batch
  (default: 200; maximum: 500).}

  \item{rettype}{A character string specifying the record
  view returned, such as 'abstract' or 'medline' from
  PubMed, or 'gp' or 'fasta' from protein. See
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
  for allowed values for each database.}

  \item{retmode}{A character string specifying the data
  format of the records returned, such as plain text, XML,
  or asn.1. See
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
  for allowed values for each database.}

  \item{strand}{Strand of DNA to retrieve. (1: plus strand,
  2: minus strand)}

  \item{seq_start}{First sequence base to retrieve.}

  \item{seq_stop}{Last sequence base to retrieve.}

  \item{complexity}{Data content to return. (0: entire data
  structure, 1: bioseq, 2: minimal bioseq-set, 3: minimal
  nuc-prot, 4: minimal pub-set)}
}
\value{
  An \code{\link{efetch-class}} object.
}
\description{
  \code{efetch.batch} retrieves large data sets from NCBI
  in batches.
}
\details{
  See the online documentation at
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}
  for additional information.
}
\examples{
query <- 'chimpanzee[orgn] and biomol mrna[prop]'
  (s <- esearch(query, "nuccore", usehistory=TRUE))
  ## This takes some time
  f <- efetch.batch(s, chunk_size=200, rettype="fasta")
  write(f, file="~/chimp_mrna.fa")
}

