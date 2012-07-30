\name{efetch-class}
\alias{c,efetch-method}
\alias{content,efetch-method}
\alias{efetch-class}
\alias{write,efetch-method}
\title{\dQuote{efetch} class}
\usage{
  .efetch(...)

  content(x, parse = TRUE, format = c("Biostrings",
    "DNAbin", "String"))
}
\arguments{
  \item{...}{arguments passed to the constructor method}

  \item{format}{Output format of sequence data.}

  \item{x}{an \code{\link{eutil-class}} object.}
}
\description{
  efetch is an S4 class that provides a container for data
  retrived by calls to the NCBI EFetch utility.
}
\section{Slots}{
  \describe{ \item{\code{url}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{error}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{content}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{database}:}{The
  name of the queried database.} \item{\code{type}:}{A
  character vector specifying the record view returned,
  such as Abstract or MEDLINE from \sQuote{PubMed}, or
  GenPept or FASTA from \sQuote{protein}}
  \item{\code{mode}:}{A character vector specifying the
  data format of the records returned, such as plain text,
  HMTL or XML} }
}

\section{Extends}{
  Class \code{"\linkS4class{eutil}"}, directly.
}
\seealso{
  \code{\link{efetch}} for generating calls to the NCBI
  EFetch utility.
}

