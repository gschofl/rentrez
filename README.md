
```
## Loading required package: rmisc
```

```
## Loading required package: rentrez
```


# rentrez

`rentrez` provides an interface to the NCBI Entrez Utilities.

Information on the Entrez utilities can be found
[here](http://http://www.ncbi.nlm.nih.gov/books/NBK25501/).

### Installation

This package is currently only available via github. It depends on a number
of utility functions provided in my [rmisc](https://github.com/gschofl/rmisc) package. Use Hadley Wickham's [devtools](https://github.com/hadley/devtools)
to install:


```r
install_github("rmisc", "gschofl")
install_github("rentrez", "gschofl", ref = "no_parsing")
```


### Important functions

#### `esearch`

`esearch`: Searches and retrieves a list of primary UIDs or the NCBI History
Server information (queryKey and webEnv).

These can be passed on to in `epost`, `esummary`, `elink`, or `efetch`.


```r
# Get the PubMed IDs (PMIDs) for articles with Chlamydia psittaci in the
# title that have been published in 2012.
pmid <- esearch("Chlamydia psittaci[titl] and 2012[pdat]", "pubmed")
pmid
```

```
## ESearch query using the 'pubmed' database.
## Query term: 'Chlamydia psittaci[titl] AND 2012[pdat]'
## Total number of hits: 10
## Number of hits retrieved: 10
##  [1] "23098816" "22957128" "22689815" "22506068" "22472082" "22382892"
##  [7] "22302240" "22299031" "22296995" "21921110"
```

```r

# alternatively put the pmids on the history server
pmid2 <- esearch("Chlamydia psittaci[titl] and 2012[pdat]", "pubmed", usehistory = TRUE)
pmid2
```

```
## ESearch query using the 'pubmed' database.
## Query term: 'Chlamydia psittaci[titl] AND 2012[pdat]'
## Number of UIDs stored on the History server: 10
## Query Key: 1
## WebEnv: NCID_1_261107234_130.14.18.48_5555_1352970255_534246560
```


Some accessors for `esearch` objects


```r
queryUrl(pmid)
```

```
## [1] "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=Chlamydia+psittaci[titl]+AND+2012[pdat]&retstart=0&retmax=100&rettype=uilist&tool=rentrez&email=gschofl@yahoo.de"
```

```r
database(pmid)
```

```
## [1] "pubmed"
```

```r
count(pmid)
```

```
## [1] 10
```

```r
idList(pmid)
```

```
##  [1] "23098816" "22957128" "22689815" "22506068" "22472082" "22382892"
##  [7] "22302240" "22299031" "22296995" "21921110"
## attr(,"database")
## [1] "pubmed"
```

```r
content(pmid, "xml")
```

```
## <?xml version="1.0"?>
## <!DOCTYPE eSearchResult PUBLIC "-//NLM//DTD eSearchResult, 11 May 2002//EN" "http://www.ncbi.nlm.nih.gov/entrez/query/DTD/eSearch_020511.dtd">
## <eSearchResult>
##   <Count>10</Count>
##   <RetMax>10</RetMax>
##   <RetStart>0</RetStart>
##   <IdList>
##     <Id>23098816</Id>
##     <Id>22957128</Id>
##     <Id>22689815</Id>
##     <Id>22506068</Id>
##     <Id>22472082</Id>
##     <Id>22382892</Id>
##     <Id>22302240</Id>
##     <Id>22299031</Id>
##     <Id>22296995</Id>
##     <Id>21921110</Id>
##   </IdList>
##   <TranslationSet/>
##   <TranslationStack>
##     <TermSet>
##       <Term>Chlamydia psittaci[titl]</Term>
##       <Field>titl</Field>
##       <Count>702</Count>
##       <Explode>Y</Explode>
##     </TermSet>
##     <TermSet>
##       <Term>2012[pdat]</Term>
##       <Field>pdat</Field>
##       <Count>910841</Count>
##       <Explode>Y</Explode>
##     </TermSet>
##     <OP>AND</OP>
##   </TranslationStack>
##   <QueryTranslation>Chlamydia psittaci[titl] AND 2012[pdat]</QueryTranslation>
## </eSearchResult>
```




