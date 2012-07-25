\name{einfoDbList-class}
\alias{content,einfoDbList-method}
\alias{einfoDbList-class}
\title{einfoDbList class}
\usage{
  .einfoDbList(...)
}
\description{
  \sQuote{einfoDbList} is an S4 class that extends the
  \code{\link{einfo-class}}. This class provides a
  container for data retrived by calls to the
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.The_Eight_Eutilities_in_Brief}{NCBI
  EInfo} utility.
}
\details{
  einfoDbList objects have one slots in addition to the
  slots provided by basic \code{\link{eutil-class}}
  objects: \describe{ \item{dbList}{A list of the names of
  all valid Entrez databases} }
}
\seealso{
  \code{\link{einfo}} for generating calls to the NCBI
  EInfo utility.
}

