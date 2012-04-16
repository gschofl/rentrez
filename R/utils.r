### Utility functions ######################################################
##' @include blast-classes.r
##' @include eutil-classes.r
##' @importClassesFrom Biostrings XString
##' @importClassesFrom Biostrings XStringSet
##' @import XML
##' @import RCurl
NULL

#' Construct url, fetch response, construct eutil object
#' 
#' @param eutil Which eutil?
#' @param ... Additional args.
#' 
#' @rdname internal
#' @keywords internal
.query <- function (eutil, ...)
{
  eutils_host <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/'
  #query_string <- .query_string(...)
  query_string <- .query_string(..., tool="Rentrez", email="gschofl@yahoo.de")
  
  if (identical(eutil, "egquery"))
    url <- sprintf('http://eutils.ncbi.nlm.nih.gov/gquery/%s', query_string)
  else
    url <- sprintf('%s%s.fcgi%s', eutils_host, eutil, query_string)
  
  if (identical(eutil, "efetch"))
    new("eutil",
        url=curlUnescape(url),
        data=getURL(url))
  else
    new("eutil",
        url=curlUnescape(url),
        data=xmlTreeParse(getURL(url), useInternalNodes=TRUE))
}

.query_string <- function (...)
{
  args <- list(...)
  params <- names(args)
  empty <- sapply(args, is.null)
  fields <- paste(as.character(params[!empty]), as.character(args[!empty]), sep="=")
  .escape(paste("?", paste(fields, collapse="&"), sep=""))
}

.escape <- function (s, httpPOST=FALSE)
{
  if (httpPOST) {
    s <- gsub(" +", " ", s)
    s <- gsub("+", " ", s, fixed=TRUE)
  }
  else {
    s <- gsub(" +", "\\+", s)
  }
  s <- paste(strsplit(s, '\"', fixed=TRUE)[[1L]], collapse="%22")
  s <- gsub(">", "%3E", s)
  s <- gsub("\\n", "%0D%0A", s)
  s <- gsub("\\|", "%7C", s)
  s <- gsub("\\#", "%23", s)
  s <- gsub("\\+(and)\\+|\\+(or)\\+|\\+(not)\\+","\\+\\U\\1\\U\\2\\U\\3\\+", s, perl=TRUE)
  s
}

#' use HTTP POST
#' 
#' @inheritParams .query
#' 
#' @rdname internal
#' @keywords internal
.httpPOST <- function (eutil, ...)
{
  
  user_agent <- switch(eutil,
                       elink='elink/1.0',
                       esearch='esearch/1.0',
                       epost='epost/1.0',
                       esummary='esummary/1.0',
                       efetch='efetch/1.0')
  
  http_post_url <- switch(eutil,
                          elink='elink.fcgi?',
                          esearch='esearch.fcgi?',
                          epost='epost.fcgi?',
                          esummary='esummary.fcgi?',
                          efetch='efetch.fcgi?')
  
  doc <- postForm(paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils',
                 http_post_url, sep="/"), ..., type='POST',
                  .opts=curlOptions(useragent=user_agent))
  
  if (identical(eutil, "efetch")) {
    new('eutil', url='HTTP_POST', data=as.character(doc))
  }
  else {
    new('eutil',
        url='HTTP_POST',
        data=xmlTreeParse(doc, useInternalNodes=TRUE))
  }

}

# Parse a DocSum recursively and return it as a named list
.parseDocSum <- function (ds)
{
  if (xmlName(ds) == "DocSum") {
    .docsum <- function (ds) {
      items <- 
        xmlChildren(ds, addNames=FALSE)[names(xmlChildren(ds)) == "Item"]      
      value <- 
        lapply(items, function (item) {
          if (all(xmlSApply(item, xmlSize) == 0L))
            xmlValue(item)
          else
            .docsum(item)
        })
      names(value) <- 
        lapply(items, function (item) xmlGetAttr(item, "Name"))
      return(value)
    }
    return(.docsum(ds))
  }
  else if (xmlName(ds) == "DocumentSummary") {
    .docsum <- function (ds) {
      items <- 
        xmlChildren(ds, addNames=TRUE)
      value <- 
        lapply(items, function (item) {
          if (all(xmlSApply(item, xmlSize) == 0L))
            xmlValue(item)
          else
            .docsum(item)
        })
      names(value) <- lapply(items, xmlName)
      return(value)
    }
    return(.docsum(ds))
  }
}

# Parse IdCheckList returned from cmd=ncheck
.parseIdCheckList <- function (data=o@data)
{
  data <- xmlRoot(data)
  dbFrom <- xpathSApply(data, "//DbFrom", xmlValue)
  id <- xpathSApply(data, "//Id", xmlValue)
  has_neighbor <- xpathSApply(data, "//Id", xmlGetAttr, "HasNeighbor")
  
  chklst <- data.frame(stringsAsFactors=FALSE, Id=id,
                       HasNeighbor=ifelse(has_neighbor == "Y", TRUE, FALSE))
  chklst
}

# Parse a LinkSet and return it as a data.frame
.parseIdLinkSet <- function (data)
{
  data <- xmlRoot(data)
  dbFrom <- xpathSApply(data, "//DbFrom", xmlValue)
  idLinkSet <- getNodeSet(xmlRoot(data), "//IdLinkSet")
  
  if (length(idLinkSet) < 1L)
    return(list())
  
  ll <- lapply(idLinkSet, function (ls) {
    ls <- xmlDoc(ls)
    Id <- xpathSApply(ls, "(//Id)[1]", xmlValue)
    link_info <- 
      lapply(getNodeSet(ls, "//LinkInfo"), function (li) {
        li <- xmlDoc(li)
        li <- list(DbTo=xpathSApply(li, "//DbTo", xmlValue), 
                   LinkName=xpathSApply(li, "//LinkName", xmlValue),
                   MenuTag=xpathSApply(li, "//MenuTag", xmlValue),
                   HtmlTag=xpathSApply(li, "//HtmlTag", xmlValue),
                   Priority=xpathSApply(li, "//Priority", xmlValue))
        li[vapply(li, length, integer(1)) == 0L] <- NA_character_
        li
      })
    data.frame(stringsAsFactors=FALSE, Id=Id, 
               do.call(rbind, link_info))
  })
  
  ll
}

# Parse a LinkSet and return it as a named list
.parseLinkSet <- function (data)
{
  linkSetDb <- getNodeSet(xmlRoot(data), "//LinkSetDb")
  
  if (length(linkSetDb) < 1L)
    return(list())
  
  ll <- lapply(linkSetDb, function(lsd) {
    lsd <- xmlDoc(lsd)
    id <- xpathSApply(lsd, "//Id", xmlValue)
    score <- xpathSApply(lsd, "//Score", xmlValue)
    ans <- list(id=id, score=score)
    ans[vapply(ans, length, integer(1)) == 0L]  <- NULL
    ans
  })
  
  names(ll) <- xpathSApply(xmlRoot(data), "//LinkName", xmlValue)
  ll
}

checkErrors <- function (obj)
{
  error <- NULL
  err_msgs <- NULL
  wrn_msgs <- NULL
  
  err_node <- getNodeSet(slot(obj, "data"), '//ERROR')
  if (length(err_node) > 0)
    error <- lapply(err_node, xmlValue)
  
  err_list_node <- getNodeSet(slot(obj, "data"), '//ErrorList')
  if (length(err_list_node) > 0)
    err_msgs <- lapply(xmlChildren(err_list_node[[1]]), xmlValue)
  
  wrn_list_node <- getNodeSet(slot(obj, "data"), '//WarningList')
  if (length(wrn_list_node) > 0)
    wrn_msgs <- lapply(xmlChildren(wrn_list_node[[1]]), xmlValue)
  
  if (!is.null(error))
    message('Error:\n\t', unlist(error))
  
  if (!is.null(err_msgs))
    message('Error(s):\n\t', 
            paste(paste(names(err_msgs), err_msgs, sep="\t"), collapse="\n\t"))
  
  if (!is.null(wrn_msgs))
    message('Warning(s):\n\t', 
            paste(paste(names(wrn_msgs), wrn_msgs, sep="\t"), collapse="\n\t"))
  
  return(invisible(list(err=error, errmsg=err_msgs, wrnmsg=wrn_msgs)))
}

.getDb <- function (object)
{
  if (is(object, "esearch") || is(object, "epost") || is(object, "idlist"))
    db <- object@database
  else if (is(object, "elink"))
    db <- object@databaseTo
  else
    db <- NULL
  db
}

.getId <- function (object)
{
  # we need the count basically for efetch.batch
  if (is(object, "epost")) {
    WebEnv <- object@webEnv
    query_key <- object@queryKey
    count <- object@count
    id <- NULL
  }
  else if (is(object, "elink")) {
    if (is.na(object@queryKey) && is.na(object@webEnv)) {
      WebEnv <- NULL
      query_key <- NULL
      count <- length(unlist(object@linkList))
      id <- unlist(object@linkList)
    } else {
      WebEnv <- object@webEnv
      query_key <- object@queryKey
      count <- NA # we don't have any clue how many UIDs are stored at the
                 # history server if we use elink with usehistory=TRUE. 
      id <- NULL
    }
  }
  else if (is(object, "esearch") || is(object, "idlist")) {
    if (is.na(object@queryKey) && is.na(object@webEnv)) {
      WebEnv <- NULL
      query_key <- NULL
      count <- length(object@idList)
      id <- object@idList
    } else {
      WebEnv <- object@webEnv
      query_key <- object@queryKey
      count <- object@count
      id <- NULL
    }
  }
  else if (is.atomic(object)) {
    WebEnv <- NULL 
    query_key <- NULL
    count <- length(object)
    id <- object
  }
  else
    stop("UIDs must be provided as a vector or as esearch objects.")
  
  return(list(WebEnv=WebEnv, query_key=query_key, count=count, id=id))
}

is.empty <- isEmpty <- function (x)
{
  if (length(x) == 0L)
    TRUE
  else if (is.character(x) && !nzchar(x))
    TRUE
  else
    FALSE
}

isFALSE <- function (x) identical(FALSE, x)

.collapse <- function (id) paste0(id, collapse = ",")

#' Flatten (Nested) Lists.
#'
#' Flatten \code{lists} according to specifications made via
#' \code{start_after} and/or \code{stop_at}. When keeping 
#' the defaults, the function will traverse \code{src} to retrieve the
#' values at the respective bottom layers/bottom elements. These values are
#' arranged in a named \code{list} where the respective names can be
#' interpreted as the the paths to the retrieved values.   
#'
#' @param x An arbitrarily deeply nested \code{list}
#' @param start_after An \code{integer} specifying the layer after which to 
#' start the flattening. \code{NULL} means to start at the very top.
#' @param stop_at An \code{integer} specifying the layer at which to stop
#' the flattening. \code{NULL} means there is not stop criterion.
#' @param delim_path A \code{character} specifying how the names
#' of the resulting flattened list should be pasted.
#' @param ... Further args.
#' @return A named \code{list} that features the desired degree of flattening.
#' @keywords internal
#' @author Janko Thyson \email{janko.thyson.rstuff@@googlemail.com}
#' @examples
#'  ##
flatten <- function (x, 
                     start_after=NULL, 
                     stop_at=NULL, 
                     delim_path=".",
                     do_warn=TRUE,
                     ... )
{
  # VALIDATE
  if (!is.list(x)) {
    stop("'src' must be a list.")
  }
  if (!is.null(start_after) && !is.null(stop_at)) {
    if (start_after == 1 && stop_at == 1)
      stop(sprintf("Invalid specification:\nstart_after: %s\nstop_at: %s\n",
                   start_after, stop_at))
  }

  # INNER FUNCTIONS
  .startAfterInner <- function(envir, nms, out.1, ...)
  {
    idx_diff <- diff(c(envir$start_after, length(envir$counter)))

    # UPDATE IF DEGREE OF NESTEDNESS EXCEEDS START CRITERION
    if (idx_diff > 0) {
      idx_cutoff <-
        seq(from=(length(envir$counter) - idx_diff + 1), to=length(envir$counter))
      
      idx_left        <- envir$counter[-idx_cutoff]
      nms.1           <- nms[idx_cutoff]
      names(out.1)    <- paste(nms.1, collapse=envir$delim_path)
      # UPDATE SRC
      idx_append <- sapply(envir$history, function (x_hist) {
        all(idx_left == x_hist)        
      })

      if (any(idx_append)) {                                          
        envir$src[[idx_left]] <- append(envir$src[[idx_left]], values=out.1)                    
      }
      else {
        envir$src[[idx_left]] <- out.1
        # UPDATE HISTORY
        envir$history <- c(envir$history, list(idx_left))
      }
      envir$out <- envir$src          
    } 
    else if (idx_diff < 0) {
      envir$out <- envir$src
    }
    
    # RESET
    envir$nms <- envir$nms[-length(envir$nms)]
    envir$counter <- envir$counter[-length(envir$counter)]
    
    return(TRUE)
  }
  
  .updateOutInner <- function (envir, out.1, ...)
  {

    # UPDATE OUT
    envir$out <- c(get("out", envir = envir), out.1)

    # RESET
    envir$nms       <- envir$nms[-length(envir$nms)]
    envir$counter   <- envir$counter[-length(envir$counter)]
    
    return(TRUE)
  }
  
  .flattenInner <- function(x, envir, ...)
  {
    if ( is(x, "list") && length(x) != 0 ) {
      
      # UPDATE
      envir$counter_history <- c(envir$counter_history, list(envir$counter))
      
      # EXIT IF DEGREE EXCEEDS CUTOFF
      if (!is.null(envir$stop_at)) {
        if (length(envir$counter) > envir$stop_at) { 
          nms <- get("nms", envir=envir)
          out.1 <- list(x)
          names(out.1) <- paste(nms, collapse=envir$delim_path)

          # DECISION ON FLATTENING
          if (!is.null(envir$start_after)) {
            .startAfterInner(envir=envir, nms=nms, out.1=out.1)
            return(NULL)
          }
          else {
            .updateOutInner(envir=envir, out.1=out.1)
            return(NULL)
          }
        }
      }

      # LOOP OVER ELEMENTS
      for (i in seq_along(x)) {
        # UPDATE COUNTER
        envir$counter <- c(envir$counter, i)
        # UPDATE NAMES
        list_names <- if (is.null(names(x[i]))) paste0("X", i) else names(x[i])
        assign("nms", c(get("nms", envir=envir), list_names), envir=envir)
        # RECURSIVE FLATTENING
        .flattenInner(x=x[[i]], envir) # call  recursively
        # RESET COUNTER
        if (i == length(x)) {
          envir$nms <- envir$nms[-length(envir$nms)]
          envir$counter <- envir$counter[-length(envir$counter)]
        }
      }
    } 
    else {

      nms <- get("nms", envir=envir)
      out.1 <- list(x)
      names(out.1) <- paste(nms, collapse=envir$delim_path)

      # DECISION ON FLATTENING
      if (!is.null(envir$start_after))
        .startAfterInner(envir=envir, nms=nms, out.1=out.1)
      else
        .updateOutInner(envir=envir, out.1=out.1)
    }
        
    return(TRUE)
  }
  
  out                     <- list()
  # ENVIR
  envir                   <- new.env()
  envir$counter           <- NULL
  envir$counter_history   <- NULL
  envir$delim_path        <- delim_path
  envir$do_warn           <- do_warn
  envir$do_block_warning  <- FALSE
  envir$history           <- NULL
  envir$nms               <- NULL
  envir$out               <- list()
  envir$src               <- x
  envir$start_after       <- start_after
  
  if (!is.null(stop_at)) {
    stop_at_0 <- stop_at
    if (stop_at == 1) {
      return(src)
    } else {
      stop_at <- stop_at - 1
    }
  }
  
  envir$stop_at           <- stop_at

  .flattenInner(x, envir)
  
  if (envir$do_warn) {
    max_length <- max(sapply(envir$counter_history, length))

    if (!is.null(start_after)) {            
      if (start_after > max_length) {                        
        warning(paste("Argument 'start_after=", start_after, 
                      "' exceeds maximum degree of sublayer nestedness (=", 
                      max_length, ").", sep=""))
      }
    }
    if (!is.null(stop_at)) {
      if (stop_at_0 > max_length){
        warning(paste("Argument 'stop_at=", stop_at_0, 
                      "' exceeds maximum degree of sublayer nestedness (=", 
                      max_length, ").", sep=""))    
      }
    }
  }
  
  out <- envir$out
  return(out)    
}


##' display a html file in a browser
##' 
##' @param html file path or html encoded character string
##' @param browser browser
##' @param unlink remove temporary file
##' 
##' @export 
displayHTML <- function (html, browser=getOption("browser"), unlink=TRUE)
{
  if (!file.exists(html)) { 
    f_tmp <- tempfile(fileext=".html")
    writeLines(html, f_tmp)
  } else {
    f_tmp <- html
    unlink <- FALSE
  }
  
  browseURL(url=f_tmp, browser=browser)
  
  if (unlink) {
    Sys.sleep(2)
    unlink(f_tmp)
  }
}


##' Format paragraphs
##' 
##' Similar to \code{\link{strwrap}} but returns a single string with
##' linefeeds inserted
##' 
##' @param s a character vector or a list of character vectors
##' @param width a positive integer giving the column for inserting
##' linefeeds
##' @param indent an integer giving the indentation of the first line of
##' the paragraph; negative values of \code{indent} are allowed and reduce
##' the width for the first line by that value.
##' @param offset a non-negative integer giving the indentation of all
##' but the first line
##' @param split regular expression used for splitting. Defaults to
##' a whitespace character.
##' @param FORCE if \code{TRUE} words are force split if the available width
##' is too small.
##' @param FULL_FORCE Always split at the specified position.
##' 
##' @return a character vector
##' @keywords internal
linebreak <- function (s, width=getOption("width") - 2, indent=0, offset=0,
                       split=" ", FORCE=FALSE, FULL_FORCE=FALSE) {
  if (!is.character(s)) 
    s <- as.character(s)
  
  if (length(s) == 0L)
    return("")
  
  # set indent string to "" if a negative value is given
  # this lets us shrink the available width for the first line by that value
  indent_string <- blanks(ifelse(indent < 0, 0, indent))
  offset_string <- paste0("\n", blanks(offset))
  
  s <- mapply(function (s, width, offset, indent, indent_string, split, FORCE, FULL_FORCE) {
    # remove leading and trailing blanks
    # convert newlines, tabs, spaces to " "
    # find first position where 'split' applies
    if (!FULL_FORCE) {
      s <- gsub("[[:space:]]+", " ", gsub("^[[:blank:]]+|[[:blank:]]+$", "", s), perl=TRUE)
    }
    fws <- regexpr(split, s, perl=TRUE)
    if (offset + indent + nchar(s) > width) {
      # if not everything fits on one line
      if (FULL_FORCE ||
         (fws == -1 || fws >= (width - offset - indent)) && FORCE) {
        # if no whitespace or first word too long and force break
        # cut through the middle of a word
        pat1 <- paste0("^.{", width - offset - indent, "}(?=.+)")
        pat2 <- paste0("(?<=^.{", width - offset - indent, "}).+")
        leading_string <- regmatches(s, regexpr(pat1, s, perl=TRUE))
        trailing_string <- regmatches(s, regexpr(pat2, s, perl=TRUE)) 
        s <- paste0(indent_string, leading_string, offset_string,
                    linebreak(s=trailing_string, width=width, indent=0,
                              offset=offset, split=split, FORCE=FORCE,
                              FULL_FORCE=FULL_FORCE))
      } 
      else if ((fws == -1 || fws >= (width - offset + indent)) && !FORCE) {
        # if no whitespace or first word too long and NO force break
        # stop right here
        stop("Can't break in the middle of a word. Use the force!")
      }
      else {
        # break the line
        s_split <- unlist(strsplit(s, split))
        s_cum <- cumsum(nchar(s_split) + 1)
        leading_string <- 
          paste0(s_split[s_cum < width - offset - indent],
                 ifelse(split == " ", "", split), collapse=split)
        trailing_string <- 
          paste0(s_split[s_cum >= width - offset - indent], collapse=split)
        s <- paste0(indent_string, leading_string, offset_string,
                    linebreak(s=trailing_string, width=width, indent=0,
                              offset=offset, split=split, FORCE=FORCE, FULL_FORCE=FULL_FORCE))
      }
    }
    else
      # if everything fits on one line go with the string
      s
  }, s, width, offset, abs(indent), indent_string, split, FORCE, FULL_FORCE,
              SIMPLIFY=FALSE, USE.NAMES=FALSE)
  unlist(s)
}


##' create blank strings with a given number of characters
##' @seealso Examples for \code{\link{regmatches}}
##' @keywords internal
blanks <- function(n) {
  vapply(Map(rep.int, rep.int(" ", length(n)), n, USE.NAMES=FALSE),
         paste, "", collapse="")
}

##' @importFrom stringr str_pad
##' @keywords internal
wrapAln <- function (seq1,
                     ..., 
                     prefix=c(""),
                     suffix=c(""),
                     start=c(1),
                     reverse=c(FALSE),
                     sep=2)
{
  
  seqs <- c(seq1, list(...))
  lseqs <- sapply(seqs, nchar)
  
  if (!length(unique(lseqs)) == 1L)
    stop("Sequences are of different length")
  
  pref_width <- max(sapply(prefix, nchar))
  aln_start_width <- aln_end_width <- nchar(as.character(unique(lseqs)))
  suf_width <- max(sapply(suffix, nchar))
  offset <- pref_width + sep + aln_start_width + 1 + 1 + aln_end_width + sep + suf_width  
  
  # break up sequences  
  s <- linebreak(seqs, getOption("width") - offset, FULL_FORCE=TRUE)
  s <- str_split(s, "\n")  
  seq_widths <- nchar(s[[1L]])
  max_seq_width <- max(seq_widths)
  
  seq_starts <-
    mapply(function (start, rev) {
      x <- Reduce("+", seq_widths, init=start, right=rev, accumulate=TRUE)
      x <- x[-which.max(x)]
      x
    }, start=start, rev=reverse, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  new_starts <- 
    mapply( function (s, rev) if (rev) s[length(s) - 1] - 1 else s[2] - 1,
            s=seq_starts, rev=reverse)
  
  seq_ends <-
    mapply( function (start, rev) {
      x <- Reduce("+", seq_widths, init=start, right=rev, accumulate=TRUE)
      x <- x[-which.max(x)]
    }, start=new_starts, rev=reverse, SIMPLIFY=FALSE, USE.NAMES=FALSE)  
  
  tmp <- seq_ends[reverse]
  seq_ends[reverse] <- seq_starts[reverse]
  seq_starts[reverse] <- tmp
 
  pasteAln <- function(prefix, seq_starts, s, seq_ends, suffix) {
    seq_starts[is.empty(seq_starts)] <- ""
    seq_ends[is.empty(seq_ends)] <- ""
    paste0(str_pad(prefix, pref_width, side="right"),
           blanks(sep),
           str_pad(as.character(seq_starts), aln_start_width, side="left"),
           blanks(1),
           str_pad(s, max_seq_width, side="right"),
           blanks(1),
           str_pad(as.character(seq_ends), aln_start_width, side="left"),
           blanks(sep),
           str_pad(suffix, suf_width, side="right"))
  }
  
  s <- mapply(pasteAln, prefix=prefix, seq_starts=seq_starts, s=s,
              seq_ends=seq_ends, suffix=suffix,
              SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  s <- paste0(do.call(function (...) paste(..., sep="\n"), s),
              collapse="\n\n")
  s
}


