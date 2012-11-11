#' @include utils.r
#' @include eutil.r
NULL

#' taxonList
#' 
#' An extension of \link{list} that holds only \linkS4class{taxon} instances.
#' 
#' @export
#' @classHierarchy
#' @classMethods
setClass("taxonList", contains="list")


#' @keywords internal
setClass("Lineage", contains="taxonList")


#' taxon
#'
#' \dQuote{taxon} is an S4 class that provides a container for Entrez
#' records retrieved from the NCBI Taxonomy database.
#' 
#' @slot taxId A \linkS4class{character} vector containing the Taxonomy
#'  Identifier, a stable unique identifier for each taxon in
#'  the NCBI Taxonomy database.
#' @slot parentTaxId The Taxonomy Identifier of the parental taxon or
#'  \code{NULL}.
#' @slot scientificName The unique scientific name of the taxon.
#' @slot otherName A named character vector holdoing synonyms, GenBankSynonyms
#'  or \code{NULL}.
#' @slot authority
#' @slot typeMaterial
#' @slot rank
#' @slot lineage
#' @slot content
#'  
#' @rdname taxon
#' @export
#' @classHierarchy
#' @classMethods
setClass("taxon",
         representation(taxId = "character",
                        parentTaxId = "characterOrNull",
                        scientificName = "character",
                        otherName = "characterOrNull",
                        authority = "characterOrNull",
                        typeMaterial = "characterOrNull",
                        rank = "character",
                        lineage = "Lineage",
                        content = "XMLOrChar"),
         prototype(taxId = NA_character_,
                   parentTaxId = NULL,
                   scientificName = NA_character_,
                   otherName = NULL,
                   authority = NULL,
                   typeMaterial = NULL,
                   rank = NA_character_,
                   lineage = new("Lineage"),
                   content = NA_character_))


#' @details
#' See the official documentation at NCBI
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK21100/}{NCBI}
#' for more information.
#' 
#' @param term A vector of valid NCBI search terms or \sQuote{taxids}.
#'
#' @return A \linkS4class{taxon} or \linkS4class{taxonList} instance.
#' @rdname taxon
#' @export
taxon <- function (term) {
  if (missing(term)) {
    return(new("taxon"))
  }
  if (is.numeric(term) || !any(is.na(suppressWarnings(as.numeric(term))))) {
    field <- "Taxonomy ID"
  } else {
    field <- "All Names"
  }
  tx_search <- esearch(term, "taxonomy", field=field, usehistory=TRUE)
  if (count(tx_search) == 0) {
    return(new("taxon"))
  } else {
    content(efetch(tx_search)) 
  }
}

# taxId methods ----------------------------------------------------------


setMethod("taxId", "taxon", function (x, use.names = TRUE) {
  if (use.names)
    setNames(x@taxId, nm=sciName(x))
  else
    x@taxId
})

setMethod("taxId", "taxonList", function (x, use.names = TRUE) {
  if (use.names)
    setNames(vapply(x, taxId, FUN.VALUE=character(1)),
             nm=sciName(x))
  else 
    vapply(x, taxId, FUN.VALUE=character(1))
})


# sciName methods --------------------------------------------------------


setMethod("sciName", "taxon", function (x) x@scientificName)

setMethod("sciName", "taxonList", function (x) {
  vapply(x, sciName, FUN.VALUE=character(1))
})


# taxRank methods --------------------------------------------------------


setMethod("taxRank", "taxon", function(x) x@rank)

setMethod("taxRank", "taxonList", function(x) {
  vapply(x, taxRank, FUN.VALUE=character(1))
})


# other accessors --------------------------------------------------------


setMethod("synonym", "taxon", function(x) x@otherName)

setMethod("authority", "taxon", function(x) x@authority)

setMethod("lineage", "taxon", function(x) x@lineage)

setMethod("parent", "taxon", function(x) {
  taxon(x@parentTaxId)
})


# show methods -----------------------------------------------------------


setMethod("show", "taxon",
          function (object) {
            lin <- paste(sciName(lineage(object)), collapse="; ")
            showme <- sprintf("%s\nTaxonomy Id: %s; Rank: %s\n",
                              sciName(object), sQuote(taxId(object, FALSE)),
                              sQuote(taxRank(object))) 
            cat(showme)
            
            if (not_empty(lin))
              cat(sprintf("Lineage: %s\n", lin))
          })


setMethod("show", "taxonList",
          function (object) {
            lo <- length(object)
            cat(sprintf("A %s instance of length %s", sQuote(class(object)), lo))
            showme <- sprintf("\n%s (%s; %s)", sciName(object), taxId(object, FALSE),
                              taxRank(object))
            cat(showme)
          })


setMethod("show", "Lineage",
          function (object) {
            lo <- length(object)
            lin <- paste(sciName(object), collapse="; ")
            cat(sprintf("A %s of length %s\n%s", sQuote(class(object)), lo, lin))   
            return(invisible(lin))
          })
