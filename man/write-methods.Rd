\name{write,efetch-method}
\alias{write,efetch-method}
\title{Write efetch data to file}
\usage{
  write(x, file = "data", append = FALSE)
}
\arguments{
  \item{x}{An \code{efetch} object.}

  \item{file}{A connection, or a character string naming
  the file to write to.}

  \item{append}{if \code{TRUE} the data \code{x} is
  appended to the connection}

  \item{ncolumns}{the number of columns to write the data
  in.}

  \item{sep}{a string used to separate columns.  Using
  \code{sep = "\t"} gives tab delimited output; default is
  \code{" "}.}
}
\description{
  Write efetch data to file
}

