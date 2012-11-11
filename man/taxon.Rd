% Built by roxygen3 0.1
\docType{class}
\name{taxon-class}
\alias{taxon}
\alias{taxon-class}
\title{taxon}
\usage{
  taxon(term)
}
\arguments{
  \item{term}{A vector of valid NCBI search terms or \sQuote{taxids}.}
}

\value{
A \linkS4class{taxon} or \linkS4class{taxonList} instance.
}
\description{
\dQuote{taxon} is an S4 class that provides a container for Entrez
records retrieved from the NCBI Taxonomy database.
}
\details{
See the official documentation at NCBI
\href{http://www.ncbi.nlm.nih.gov/books/NBK21100/}{NCBI}
for more information.
}
\section{Generics with methods for taxon}{
\itemize{
  \item \code{\link{authority}}
  \item \code{\link{lineage}}
  \item \code{\link{parent}}
  \item \code{\link{sciName}}
  \item \code{\link{show}}
  \item \code{\link{synonym}}
  \item \code{\link{taxId}}
  \item \code{\link{taxRank}}
}

}

\keyword{classes}
\section{Slots}{
\describe{
  \item{taxId}{A \linkS4class{character} vector containing the Taxonomy
 Identifier, a stable unique identifier for each taxon in
 the NCBI Taxonomy database.}

  \item{parentTaxId}{The Taxonomy Identifier of the parental taxon or
 \code{NULL}.}

  \item{scientificName}{The unique scientific name of the taxon.}

  \item{otherName}{A named character vector holdoing synonyms, GenBankSynonyms
 or \code{NULL}.}

  \item{authority}{An object of class \linkS4class{characterOrNull}}

  \item{typeMaterial}{An object of class \linkS4class{characterOrNull}}

  \item{rank}{An object of class \linkS4class{character}}

  \item{lineage}{An object of class \linkS4class{Lineage}}

  \item{content}{An object of class \linkS4class{XMLOrChar}}

}
}
