# `rentrez`

`rentrez` provides an interface to the NCBI Entrez Utilities.

Information on the Entrez utilities can be found
[here](http://http://www.ncbi.nlm.nih.gov/books/NBK25501/).

## Installation

This package is currently only available via github. It depends on a number
of utility functions provided in my [rmisc](https://github.com/gschofl/rmisc) package. Use Hadley Wickham's [devtools](https://github.com/hadley/devtools)
to install:


```r
install_github("rmisc", "gschofl")
install_github("rentrez", "gschofl", ref = "no_parsing")
```

