## convenience methods for bibentries ####

#' Open a bibentry in the browser
#' 
#' @param ref A \code{\link[utils]{bibentry}} object
#' @param ... Further arguments
#' 
#' @export
browse <- function (ref, ...) {
  UseMethod("browse", ref)
}

#' @export
browse.bibentry <- function (ref, browser = getOption("browser")) {  
  if (all(!nzchar(ref$doi))) {
    return("No doi available")
  }
  l <- lapply(ref$doi[nzchar(ref$doi)], function (doi) {
    browseURL(paste0('http://dx.doi.org/', doi), browser = browser)
  })
  invisible()
}

#' Access abstract from a bibentry
#' 
#' @param ref A \code{\link[utils]{bibentry}} object
#' @param ... Further arguments
#' 
#' @export
abstract <- function (ref, ...) {
  UseMethod("abstract", ref)
}

#' @export
abstract.bibentry <- function (ref) {
  return(ref$abstract)
}
