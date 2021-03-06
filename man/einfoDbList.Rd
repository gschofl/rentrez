% Built by roxygen3 0.1
\docType{class}
\name{einfoDbList-class}
\alias{einfoDbList-class}
\title{einfoDbList}
\description{
\dQuote{einfoDbList} is an S4 class that provides a container for data
retrived by calls to the 
\href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI EInfo}
utility.
}
\section{Class hierarchy for einfoDbList}{
Super classes:
\itemize{
  \item \linkS4class{einfo}
  \item \linkS4class{eutil}
}

}

\section{Generics with methods for einfoDbList}{
\itemize{
  \item \code{\link{[}}
}

}

\keyword{classes}
\section{Slots}{
\describe{
  \item{url}{A character vector containing the query URL.}

  \item{error}{Any error or warning messages parsed from
the output of the call submitted to Entrez.}

  \item{content}{A character vector holding the unparsed
contents of a request to Entrez.}

  \item{dbName}{A list of the names of all valid Entrez databases.}

}
}
