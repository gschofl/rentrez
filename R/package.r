##' The rentrez package provides an interface to NCBI's
##' Entrez utilities.
##'
##' NCBI provides the EUtilities web service for software to query the Entrez
##' databases directly, rather than through the web interface. For more
##' information see \href{http://www.ncbi.nlm.nih.gov/books/NBK25500/}{here}.
##' 
##' This package provides an interface to construct query URLs and parse 
##' query results.
##'
##' Important functions:
##'   \code{\link{einfo}}: Provides field names, term counts, last update,
##'   and available updates for each database.
##'   \code{\link{esearch}}: Searches and retrieves primary UIDs (for use
##'   with \code{epost}, \code{esummary}, \code{elink}, or \code{efetch})
##'   \code{\link{epost}}: Uploads primary UIDs to the Entrez history server
##'   for subsequent use with e.g. \code{esummary}, \code{elink}, or 
##'   \code{efetch})
##'   \code{\link{esummary}}: Retrieves document summaries (DocSums) from
##'   a list of primary UIDs.
##'   \code{\link{elink}}: Returns a list of UIDs (and relevancy scores) from
##'   a target database that are related to a list of UIDs in the same
##'   database or in another Entrez database.
##'   \code{\link{efetch}}: Retrieves data records in a specified format
##'   corresponding to a list of UIDs.
##'   
##' @example inst/examples/package.r
##'
##' @name rentrez-package
##' @author Gerhard Schoefl \email{gschofl@@yahoo.de}
##' @docType package
##' @import XML
##' @keywords package
NA
####



# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

