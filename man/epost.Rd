% Built by roxygen3 0.1
\docType{class}
\name{epost-class}
\alias{epost}
\alias{epost-class}
\title{epost}
\usage{
  epost(id, db = NULL, WebEnv = NULL)
}
\arguments{
  \item{id}{(Required) List of UIDs.}

  \item{db}{(Required) Database containing the UIDs in the input list.}

  \item{WebEnv}{(Optional) Web Environment. If provided, this parameter
specifies the Web Environment that will receive the UIDs sent by
\code{epost}. EPost will create a new query key associated with that
Web Environment. The WebEnv value is usually returned by a previous
call to \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
If no WebEnv parameter is provided, EPost will create a new Web 
Environment and post the UIDs to query_key 1.}
}

\value{
An \code{epost} instance.
}
\description{
\dQuote{epost} is an S4 class that provides a container for data retrived
by calls to the NCBI EPost utility.

\code{epost} allows to upload a list of UIDs to the Entrez 
History Server and returns an integer label called a query key and
an encoded cookie string called a Web environment.
\code{epost} instances can then be used in stead of a UID
list in subsequent \code{\link{esummary}}, \code{\link{efetch}}, or
\code{\link{elink}} calls.
}
\details{
See the official online documentation for NCBI's
\href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EPost}{EUtilities}
for additional information.
}
\section{Class hierarchy for epost}{
Super classes:
\itemize{
  \item \linkS4class{eutil}
}

}

\section{Generics with methods for epost}{
\itemize{
  \item \code{\link{content}}
  \item \code{\link{show}}
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

  \item{database}{The number of items posted.}

  \item{count}{The index of the first hit retrieved.}

  \item{queryKey}{Integer label called query key, specifying the
location on the Entrez history server of the list of UIDs matching the
Entrez query.}

  \item{webEnv}{Encoded cookie string called a Web environment,
specifying the location on the Entrez history server of the list of UIDs
matching the Entrez query.}

}
}
