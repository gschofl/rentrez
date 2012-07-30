##' The rentrez package provides an interface to NCBI's
##' Entrez utilities.
##'
##' @section description:
##'   NCBI provides the EUtilities web service for software to query the Entrez
##'   databases directly, rather than through the web interface. For more
##'   information see \href{http://www.ncbi.nlm.nih.gov/books/NBK25500/}{here}.
##' 
##'   This package provides an interface to construct query URLs and parse 
##'   query results.
##' 
##'   The main Entrez web page is available at
##'   \url{http://www.ncbi.nlm.nih.gov/Entrez/}and a list of the Entrez 
##'   utilities can be found at
##'   \url{http://www.ncbi.nlm.nih.gov/entrez/utils/utils_index.html}.
##' 
##'   Important functions:
##'   \itemize{
##'    
##'     \item \code{\link{efetch}}: Retrieves data records in a specified
##'     format corresponding to a list of primary UIDs or from the user's Web
##'     Environment.
##'   
##'     \item \code{\link{epost}}: Uploads primary UIDs to the users's Web 
##'     Environment on the Entrez history server for subsequent use with
##'     \code{esummary}, \code{elink}, or \code{efetch}.
##'   
##'     \item \code{\link{esearch}}: Searches and retrieves primary UIDs for use
##'     in \code{epost}, \code{esummary}, \code{elink}, or \code{efetch}.
##'     \code{esearch} additonally returns term translations and optionally
##'     stores results for future use in the user's Web Environment.
##'   
##'     \item \code{\link{elink}}: Returns a list of UIDs (and relevancy
##'     scores) from a target database that are related to a list of UIDs in
##'     the same database or in another Entrez database.
##'
##'     \item \code{\link{einfo}}: Provides field names, term counts, last
##'     update, and available updates for each database.
##'   
##'     \item \code{\link{esummary}}: Retrieves document summaries from
##'     a list of primary UIDs.
##'   
##'     \item \code{\link{content}}: Retrieves the query results from the
##'     \code{\linkS4class{eutil}} object returned by any of the above functions.
##'     \code{content} will attempt to parse the query results into native R
##'    objects.
##'   
##'     Typical usage is: 
##'     \preformatted{
##'         require(rentrez)
##'         pub <- efetch(id=c("19304878","14630660"), db="pubmed")
##'         records <- content(pub)
##'     }
##'   }
##'   
##' @example inst/examples/package.r
##'
##' @name rentrez-package
##' @author Gerhard Schöfl \email{gschofl@@yahoo.de}
##' @docType package
##' @keywords package
NA
####



# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

