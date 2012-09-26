\name{content}
\alias{content}
\alias{content-methods}
\title{Extract content from an eutil request.}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Arguments passed on to methods.}
}
\description{
  This function retrieves the data returned by a call to
  NCBI's Entrez Utilities from the resulting \code{eutil}
  objects. As default, the function will attempt to parse
  the contents into an R object.
}

