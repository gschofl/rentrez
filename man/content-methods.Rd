\name{content}
\alias{content}
\alias{content-methods}
\title{Extract content from an eutil request.}
\usage{
  content(x, parse = TRUE, format = c("Biostring",
    "DNAbin", "String"))
}
\arguments{
  \item{x}{an \code{\link{eutil-class}} object.}

  \item{parse}{Parse into an R object where possible.}
}
\description{
  This function retrieves the data returned by a call to
  NCBI's Entrez Utilities from the resulting \code{eutil}
  objects. As default, the function will attempt to parse
  the contents into an R object.
}

