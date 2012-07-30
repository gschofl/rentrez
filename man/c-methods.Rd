\name{c,efetch-method}
\alias{c,efetch-method}
\title{Combining efetch objects}
\usage{
  c(...)
}
\arguments{
  \item{...}{objects to be combined.}

  \item{recursive}{logical.  If \code{recursive = TRUE},
  the function recursively descends through lists (and
  pairlists) combining all their elements into a vector.}
}
\description{
  Only data retrieved from the same datebase in the same
  \code{retmode} and \code{rettype} can be combined.
}

