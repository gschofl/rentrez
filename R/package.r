##' The rentrez package provides an interface to NCBI's
##' EUtils/Entrez server.
##'
##' NCBI provides the EUtils web service for software to query the Entrez
##' databases directly, rather than through the web interface. For more
##' information see \url{http://www.ncbi.nlm.nih.gov/books/NBK25500/}.
##' 
##' This package provides an interface to construct query URLs and parse 
##' query results.
##'
##' Important functions:
##'   \code{\link{einfo}}: Provides field names, term counts, last update,
##'     and available updates for each database.
##'   \code{\link{esearch}}: Searches and retrieves primary UIDs (for use
##'      in \code{esummary} and \code{efetch})
##'   \code{\link{esummary}}: Retrieves document summaries (DocSums) from
##'       a list of primary UIDs.
##'
##' @name rentrez-package
##' @author Gerhard Schöfl \email{gschofl@@yahoo.de}
##' @references \url{www.website.de}
##' @docType package
##' @keywords package
NA
####



# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

