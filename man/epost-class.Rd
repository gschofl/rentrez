\name{epost-class}
\alias{content,epost-method}
\alias{epost-class}
\title{\dQuote{epost} class}
\usage{
  .epost(...)

  \S4method{content}{epost}(x, ...)
}
\arguments{
  \item{...}{arguments passed to the constructor method}

  \item{x}{an \code{\link{eutil-class}} object.}
}
\description{
  \dQuote{epost} is an S4 class that provides a container
  for data retrived by calls to the NCBI EPost utility.
}
\section{Slots}{
  \describe{ \item{\code{url}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{error}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{content}:}{See
  \code{\linkS4class{eutil}}.} \item{\code{database}:}{The
  number of items posted.} \item{\code{count}:}{The index
  of the first hit retrieved.}
  \item{\code{queryKey}:}{Integer label called query key,
  specifying the location on the Entrez history server of
  the list of UIDs matching the Entrez query.}
  \item{\code{webEnv}:}{Encoded cookie string called a Web
  environment, specifying the location on the Entrez
  history server of the list of UIDs matching the Entrez
  query.} }
}

\section{Extends}{
  Class \code{"\linkS4class{eutil}"}, directly.
}
\seealso{
  \code{\link{esearch}} for generating calls to the NCBI
  EPost utility.
}

