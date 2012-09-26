% Built by roxygen3 0.1
\name{content}
\alias{content}
\title{Extract content from an eutil request.}
\usage{
  content(x, ...)
}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object}

  \item{...}{Further arguments passed on to methods.}
}

\description{
This function retrieves the data returned by a call to NCBI's Entrez
Utilities from the resulting \code{eutil} objects. As default, the function
will attempt to parse the contents into an R object.
}
\section{Method signatures for generic content}{
\itemize{
  \item \linkS4class{efetch}
  \item \linkS4class{einfoDb}
  \item \linkS4class{einfoDbList}
  \item \linkS4class{elink}
  \item \linkS4class{epost}
  \item \linkS4class{esearch}
  \item \linkS4class{esummary}
}

}

\keyword{methods}
