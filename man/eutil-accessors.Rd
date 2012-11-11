% Built by roxygen3 0.1
\name{error}
\alias{count}
\alias{database}
\alias{error}
\alias{query}
\alias{queryKey}
\alias{queryTranslation}
\alias{retmax}
\alias{retmode}
\alias{retstart}
\alias{rettype}
\alias{uid}
\alias{webEnv}
\title{Eutil accessors}
\usage{
  error(x, ...)

  query(x, ...)

  retmode(x, ...)

  rettype(x, ...)

  database(x, ...)

  retmax(x, ...)

  retstart(x, ...)

  count(x, ...)

  queryTranslation(x, ...)

  uid(x, ...)

  webEnv(x, ...)

  queryKey(x, ...)
}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} instance.}

  \item{...}{Further arguments passed on to methods.}
}

\description{
Access the various slots of \linkS4class{eutil} instances.
}
\section{Method signatures for generic error}{
\itemize{
  \item \linkS4class{eutil}
}

}

\section{Method signatures for generic query}{
\itemize{
  \item \linkS4class{eutil}
}

}

\section{Method signatures for generic retmode}{
\itemize{
  \item \linkS4class{efetch}
}

}

\section{Method signatures for generic rettype}{
\itemize{
  \item \linkS4class{efetch}
}

}

\section{Method signatures for generic database}{
\itemize{
  \item \linkS4class{efetch}
  \item \linkS4class{elink}
  \item \linkS4class{epost}
  \item \linkS4class{esearch}
  \item \linkS4class{esummary}
  \item \linkS4class{webenvOrUid}
}

}

\section{Method signatures for generic retmax}{
\itemize{
  \item \linkS4class{esearch}
  \item \linkS4class{webenvOrUid}
}

}

\section{Method signatures for generic retstart}{
\itemize{
  \item \linkS4class{esearch}
  \item \linkS4class{webenvOrUid}
}

}

\section{Method signatures for generic count}{
\itemize{
  \item \linkS4class{elink}
  \item \linkS4class{epost}
  \item \linkS4class{esearch}
  \item \linkS4class{webenvOrUid}
}

}

\section{Method signatures for generic queryTranslation}{
\itemize{
  \item \linkS4class{esearch}
  \item \linkS4class{webenvOrUid}
}

}

\section{Method signatures for generic uid}{
\itemize{
  \item \linkS4class{elink}
  \item \linkS4class{esearch}
  \item \linkS4class{webenvOrUid}
}

}

\section{Method signatures for generic webEnv}{
\itemize{
  \item \linkS4class{elink}
  \item \linkS4class{epost}
  \item \linkS4class{esearch}
  \item \linkS4class{webenvOrUid}
}

}

\section{Method signatures for generic queryKey}{
\itemize{
  \item \linkS4class{elink}
  \item \linkS4class{epost}
  \item \linkS4class{esearch}
  \item \linkS4class{webenvOrUid}
}

}

\keyword{methods}
