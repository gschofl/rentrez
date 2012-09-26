% Built by roxygen3 0.1
\docType{methods}
\name{write,efetch-method}
\alias{write,efetch-method}
\title{Write efetch data to file}
\usage{
  \S4method{write}{efetch}(x, file = "data",
    ncolumns = if (is.character(x)) 1 else 5,
    append = FALSE, sep = " ")
}
\arguments{
  \item{x}{An \code{\linkS4class{efetch}} instance.}

  \item{file}{A connection, or a character string naming the file to write to.}

  \item{append}{Append the data \code{x} to the connection.}

  \item{ncolumns}{the number of columns to write the data in.}

  \item{sep}{a string used to separate columns.  Using \code{sep = "\t"}
    gives tab delimited output; default is \code{" "}.}
}

\description{
Write efetch data to file
}
\keyword{methods}
