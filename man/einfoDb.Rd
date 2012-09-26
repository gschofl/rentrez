% Built by roxygen3 0.1
\docType{class}
\name{einfoDb-class}
\alias{einfoDb-class}
\title{einfoDb}
\description{
\dQuote{einfoDb} is an S4 class that provides a container for data
retrived by calls to the
\href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI EInfo}
utility.
}
\section{Class hierarchy for einfoDb}{
Super classes:
\itemize{
  \item \linkS4class{einfo}
  \item \linkS4class{eutil}
}

}

\section{Generics with methods for einfoDb}{
\itemize{
  \item \code{\link{content}}
}

}

\keyword{classes}
\section{Slots}{
\describe{
  \item{url}{A character vector containing the query URL.}

  \item{error}{Any error or warning messages parsed from
the output of the call submitted to Entrez.}

  \item{content}{An \code{\linkS4class{XMLInternalDocument}} object or
a character vector holding the unparsed output from the call
submitted to Entrez.}

  \item{dbName}{Name of the target database.}

  \item{menuName}{Name of the target database.}

  \item{descriptiom}{Short description of the target database.}

  \item{records}{Count of records in the target database.}

  \item{lastUpdate}{Last update of the target database.}

  \item{fields}{Field names of the target database.}

  \item{links}{Available links for the target database.}

}
}
