\name{qblast}
\alias{qblast}
\title{Do a BLAST search using the qblast API}
\usage{
  qblast(query,
    program = c("megablast", "blastn", "blastp", "rpsblast", "blastx", "tblastn", "tblastx"),
    database = "nr", format = c("view", "table"),
    entrez_query = "", n_hits = 20,
    .params = list(expect = 0.001, filter = "L"),
    display = FALSE, update_time = 4)
}
\arguments{
  \item{query}{An accession number, a gi, or a fasta
  sequence provided either as an
  \code{\link[Biostrings]{XString}} or
  \code{\link[Biostrings]{XStringSet}} object, or as a
  named character vector.}

  \item{program}{Specifies the blast program to use. One of
  'megablast' (default), 'blastn', 'blastp', 'rpsblast',
  blastx', 'tblastn', 'tblastx'.}

  \item{database}{Which database to search against. E.g.
  "nr".}

  \item{format}{Output format. 'view' shows the blast
  results in a browser and returns the html response from
  the NCBI web server; 'table' returns a
  \code{\link{blast-class}} object.}

  \item{entrez_query}{An Entrez query.}

  \item{n_hits}{The maximum number of hits to return.}

  \item{.params}{Additional parameters of the qblast API.}

  \item{display}{Display the result in a web browser}

  \item{update_time}{How often to poll the blast server for
  results}
}
\value{
  A \code{\link{blast-class}} object.
}
\description{
  Run blast against the NCBI sequence databases using the
  qblast URLAPI.
}
\examples{
##
}

