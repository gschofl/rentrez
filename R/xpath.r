set_type <- function(x, as) {
  switch(as,
         character=as.character(x),
         numeric=as.numeric(x),
         integer=as.integer(x),
         double=as.double(x),
         logical=as.logical(x),
         complex=as.complex(x),
         x)
}

xvalue <- function(xdoc, path, as = 'character') {
  v <- xpathSApply(xdoc, path, xmlValue) %||% NA_character_
  set_type(v, as)
}

xname <- function(xdoc, path, as = 'character') {
  n <- xpathSApply(xdoc, path, xmlName) %||% NA_character_
  set_type(n, as)
}

xattr <- function(xdoc, path, name, as = 'character') {
  a <- xpathSApply(xdoc, path, xmlGetAttr, name=name) %||% NA_character_
  set_type(a, as)
}
