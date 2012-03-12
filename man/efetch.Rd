\name{efetch}
\alias{efetch}
\title{Retrieve data records in the requested format from NCBI}
\usage{
  efetch(id, db = attr(id, "database"), query_key = NULL,
    WebEnv = NULL, rettype = NULL, retmode = NULL,
    retstart = NULL, retmax = NULL, strand = NULL,
    seq_start = NULL, seq_stop = NULL, complexity = NULL)
}
\arguments{
  \item{id}{(Required) List of UIDs provided either as a
  character vector, as an \code{\link{esearch-class}}
  object, or by reference to a web environment and a query
  key obtained directly from the objects returned by
  previous \code{\link{esearch}} (if \code{usehistory} was
  set \code{TRUE}), \code{\link{epost}} or
  \code{\link{elink}} calls. If UIDs are provided as a
  plain character vector, \code{db} must be specified
  explicitly, and all of the UIDs must be from the database
  specified by \code{db}.}

  \item{db}{(Required only when \code{id} is a character
  vector of UIDs) Database from which to retrieve records.}

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

  \item{rettype}{A character string specifying the data
  format of the records returned, such as plain text, HMTL
  or XML. See
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
  for allowed values for each database.}

  \item{retmode}{A character string specifying the record
  view returned, such as Abstract or MEDLINE from PubMed,
  or GenPept or FASTA from protein. See
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}
  for allowed values for each database.}

  \item{retstart}{Numeric index of the first record to be
  retrieved.}

  \item{retmax}{Total number of records from the input set
  to be retrieved.}

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
  \code{efetch} retrieves data records in the requested
  format from a character vector of one or more primary
  UIDs or from a set of UIDs stored in the user's web
  environment.
}
\details{
  See the online documentation at
  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch}
  for additional information.
}
\examples{
# Fetch PMIDs 17284678 and 9997 as text abstracts
  efetch(c(17284678,9997), "pubmed", retmode="text", rettype="abstract")
}

