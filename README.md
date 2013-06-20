


# Rentrez

`Rentrez` provides an interface to the NCBI Entrez Utilities.

Information on the Entrez utilities can be found
[here](http://http://www.ncbi.nlm.nih.gov/books/NBK25501/).

### Installation

This package is currently only available via github. It depends on a number
of utility functions provided in my [rmisc](https://github.com/gschofl/rmisc) package. Use Hadley Wickham's [devtools](https://github.com/hadley/devtools)
to install:



```r
install_github("rmisc", "gschofl")
install_github("rentrez", "gschofl")
```




### Important functions

#### `esearch`

`esearch` searches and retrieves a list of primary UIDs or the NCBI History
Server information (queryKey and webEnv). The objects returned by `esearch`
can be passed on directly to `epost`, `esummary`, `elink`, or `efetch`.


#### `efetch`

`efetch` retrieves data records from NCBI in a specified retrieval type
and retrieval mode as given in this
[table](http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1). Data are returned as XML or text documents. Parsers for some data types
exist in my [ncbi](https://github.com/gschofl/ncbi) package.

#### `esummary`

`esummary` returns Entrez database summaries (DocSums) from a list of primary UIDs (Provided as a character vector or as an `esearch` object)

#### `elink`

`elink` returns a list of UIDs (and relevancy scores) from a target database
that are related to a set of UIDs provided by the user.

#### `einfo`

`einfo` provides field names, term counts, last update, and available updates
for each database.

#### `epost`

`epost` uploads primary UIDs to the users's Web Environment on the Entrez
history server for subsequent use with `esummary`, `elink`, or `efetch`.

### Examples

#### `esearch` examples

Get the PubMed IDs (PMIDs) for articles with Chlamydia psittaci in the
title that have been published in 2012.


```r
pmid <- esearch("Chlamydia psittaci[titl] and 2012[pdat]", "pubmed")
pmid

 ##  ESearch query using the 'pubmed' database.
 ##  Query term: 'Chlamydia psittaci[titl] AND 2012[pdat]'
 ##  Total number of hits: 16
 ##  Number of hits retrieved: 16
 ##   [1] "23397847" "23305020" "23265868" "23227890" "23209198" "23189195"
 ##   [7] "23098816" "22957128" "22689815" "22506068" "22472082" "22382892"
 ##  [13] "22302240" "22299031" "22296995" "21921110"
```




Alternatively put the pmids on the history server


```r
pmid2 <- esearch("Chlamydia psittaci[titl] and 2012[pdat]", "pubmed", usehistory = TRUE)
pmid2

 ##  ESearch query using the 'pubmed' database.
 ##  Query term: 'Chlamydia psittaci[titl] AND 2012[pdat]'
 ##  Number of UIDs stored on the History server: 16
 ##  Query Key: 1
 ##  WebEnv: NCID_1_8582853_165.112.9.71_9001_1371722516_533850773
```




Some accessors for `esearch` objects


```r
queryUrl(pmid)

 ##  [1] "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=Chlamydia%20psittaci%5Btitl%5D%20AND%202012%5Bpdat%5D&retstart=0&retmax=100&rettype=uilist&tool=Rentrez&email=gschofl%40yahoo.de"
```






```r
database(pmid)

 ##  [1] "pubmed"
```






```r
count(pmid)

 ##  [1] 16
```




get the list of ids as a character vector


```r
idList(pmid)

 ##   [1] "23397847" "23305020" "23265868" "23227890" "23209198" "23189195"
 ##   [7] "23098816" "22957128" "22689815" "22506068" "22472082" "22382892"
 ##  [13] "22302240" "22299031" "22296995" "21921110"
 ##  attr(,"database")
 ##  [1] "pubmed"
```




Get Query Key and Web Environment.


```r
queryKey(pmid2)

 ##  [1] 1
```






```r
webEnv(pmid2)

 ##  [1] "NCID_1_8582853_165.112.9.71_9001_1371722516_533850773"
```




Extract the content of an EUtil request as XML.


```r
content(pmid, "xml")

 ##  <?xml version="1.0"?>
 ##  <!DOCTYPE eSearchResult PUBLIC "-//NLM//DTD eSearchResult, 11 May 2002//EN" "http://www.ncbi.nlm.nih.gov/entrez/query/DTD/eSearch_020511.dtd">
 ##  <eSearchResult>
 ##    <Count>16</Count>
 ##    <RetMax>16</RetMax>
 ##    <RetStart>0</RetStart>
 ##    <IdList>
 ##      <Id>23397847</Id>
....
```




#### `efetch` examples

First we search the protein database for Chlamydia CPAF


```r
cpaf <- esearch("Chlamydia[orgn] and CPAF", "protein")
cpaf

 ##  ESearch query using the 'protein' database.
 ##  Query term: '"Chlamydia"[Organism] AND CPAF[All Fields]'
 ##  Total number of hits: 10
 ##  Number of hits retrieved: 10
 ##   [1] "339626260" "220702405" "220702404" "220702403" "220702402"
 ##   [6] "220702401" "220702400" "220702395" "220702394" "339460927"
```




Let's fetch the FASTA record for the first protein. To do that, we have to
set `rettype = "fasta"` and `retmode = "text"`. 


```r
cpaf_fasta <- efetch(cpaf[1], rettype = "fasta", retmode = "text")
cpaf_fasta

 ##  >gi|339626260|ref|YP_004717739.1| general secretion pathway protein E [Chlamydia trachomatis L2c]
 ##  MDGNKGTMQDLLDRLPYSFLKKNYLLPVEDLGDKIVFARHLKKTPLEALDEVRLITQKPLSLVSKEEAEI
 ##  IHGLQKLYSDKDGKASEMLQSMQEAVVPESESDTTELLENQENSAPVVRLLNLILKEAIEERASDIHFDP
 ##  VEDLLRIRYRIDGVLHDRHAPPNHLRAALITRIKVLTKLDIAEHRLPQDGRIKLQLGGQEIDMRVSTVPV
 ##  IHGERVVLRILDKRNVILDIRGLCMPPKMETSFRKAIGVPEGILLVTGPTGSGKTTTLYSVIQHLSGPFT
 ##  NIMTIEDPPEYKLPGVAQIAVKPKIGLTFSKGLRHLLRQDPDVLMVGEIRDQETAEIAIQAALTGHLVVS
 ##  TLHTNDAVSAIPRLLDMGVEPYLLSATMIGAVAQRLVRRICTHCKEFCVADVQEQALLRALGKDPFAPLC
 ##  KGRGCSQCFRSGYKGRQGIYEFVDVTTTLRSEIALGRPYHILRGVAEREGYCPLLEHGVELALAGETTLS
....
```




And write it to file


```r
write(cpaf_fasta, file = "~/cpaf.fna")
```




Alternatively we can fetch the FASTA records as _TSeqSet_ XML
and parse it into `AAStringSet`s using a parser function in me
[ncbi](https://github.com/gschofl/ncbi) package.


```r
cpaf_xml <- efetch(cpaf, rettype = "fasta", retmode = "xml")
cpaf_xml

 ##  <?xml version="1.0"?>
 ##  <!DOCTYPE TSeqSet PUBLIC "-//NCBI//NCBI TSeq/EN" "http://www.ncbi.nlm.nih.gov/dtd/NCBI_TSeq.dtd">
 ##  <TSeqSet>
 ##    <TSeq>
 ##      <TSeq_seqtype value="protein"/>
 ##      <TSeq_gi>339626260</TSeq_gi>
 ##      <TSeq_accver>YP_004717739.1</TSeq_accver>
 ##      <TSeq_sid>gnl|REF_ideu|CTL2C_511</TSeq_sid>
....
```






```r
require(ncbi)
aa <- parseTSeqSet(cpaf_xml)
aa

 ##    A AAStringSet instance of length 10
 ##       width seq                                         names               
 ##   [1]   500 MDGNKGTMQDLLDRLPYSFL...LALAGETTLSEVLRVAKRSE gi|339626260|gnl|...
 ##   [2]   583 SLVCKNALQDLSFLEHLLQV...NNDGTIILAEDGSFHHHHHH gi|220702405|gnl|...
 ##   [3]   583 SLVCKNALQDLSFLEHLLQV...NNDGTIILAEDGSFHHHHHH gi|220702404|gnl|...
 ##   [4]   583 SLVCKNALQDLSFLEHLLQV...NNDGTIILAEDGSFHHHHHH gi|220702403|gnl|...
 ##   [5]   583 SLVCKNALQDLSFLEHLLQV...NNDGTIILAEDGSFHHHHHH gi|220702402|gnl|...
 ##   [6]   583 SLVCKNALQDLSFLEHLLQV...NNDGTIILAEDGSFHHHHHH gi|220702401|gnl|...
....
```







