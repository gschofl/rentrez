pkgname <- "rentrez"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rentrez')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("docsum")
### * docsum

flush(stderr()); flush(stdout())

### Name: docsum
### Title: Access a DocSum from an 'esummary-class' object.
### Aliases: docsum

### ** Examples

## examples



cleanEx()
nameEx("ecount")
### * ecount

flush(stderr()); flush(stdout())

### Name: ecount
### Title: Retrieve the number of records in an Entrez database matching a
###   text query
### Aliases: ecount

### ** Examples

## retrieve the number of records for a search term in a database
n <- ecount(term="Chlamydia psittaci", db="gene")
n



cleanEx()
nameEx("efetch")
### * efetch

flush(stderr()); flush(stdout())

### Name: efetch
### Title: Retrieve data records in the requested format from NCBI
### Aliases: efetch

### ** Examples

## Download full records ###################################################

# Download nucleotide GIs 84785889 and 84785885 in FASTA format
gi_list <- c("84785889","84785885")
fasta <- efetch(id=gi_list, db="nucleotide", rettype="fasta")
fasta

# Download data from a previous search

query <- "Chlamydia psittaci and genome and 2011[pdat]"
(cpsit <- esearch(term=query, db="pubmed", usehistory=TRUE))
abstr <- efetch(cpsit, rettype="abstract")
abstr



cleanEx()
nameEx("efetch.batch")
### * efetch.batch

flush(stderr()); flush(stdout())

### Name: efetch.batch
### Title: Retrieve batches of data records in the requested format from
###   NCBI
### Aliases: efetch.batch

### ** Examples

## Download a large set of records (>500)

# Download all human ncRNAs in FASTA format
query <- "human[orgn] and biomol ncrna[prop]"

# Let's check how many there are
n <- ecount(query, "nucleotide")

# upload the GIs to the history server
ncrna <- esearch(query, "nucleotide", usehistory=TRUE)
ncrna

## Not run
## download ncRNAs in batch mode
# fasta <- efetch.batch(id=ncrna, chunk_size=200, rettype="fasta")
# write(fasta, "~/human_small_nuclear_rna.fasta")



cleanEx()
nameEx("egquery")
### * egquery

flush(stderr()); flush(stdout())

### Name: egquery
### Title: Retrieve the number of records in all Entrez databases for a
###   single text query
### Aliases: egquery

### ** Examples

## Perform a global Entrez Search

# Determine the number of records for "Chlamydia psittaci" in Entrez
egquery("Chlamydia psittaci")



cleanEx()
nameEx("einfo")
### * einfo

flush(stderr()); flush(stdout())

### Name: einfo
### Title: Retrieve information about each database in the NCBI Entrez
###   system
### Aliases: einfo

### ** Examples

## get database statistics and search fields

# a list of all valid Entrez databases
databases <- einfo()
databases

# database statistics for protein
protein <- einfo(db="protein")
protein

# search fields for protein
cbind(protein$fields$Name, strtrim(protein$fields$Description, 60))



cleanEx()
nameEx("elink")
### * elink

flush(stderr()); flush(stdout())

### Name: elink
### Title: Retrieve links to records in other Entrez databases
### Aliases: elink

### ** Examples

## Find related data through ELink #########################################

# find a set of pubmed IDs linked to nucleotide GIs
gi_list <- c("84785887","84785899","84785907","84785905","84785889")
links <- elink(id=gi_list, dbFrom="nucleotide", dbTo="pubmed")
links

esummary(links)



cleanEx()
nameEx("epost")
### * epost

flush(stderr()); flush(stdout())

### Name: epost
### Title: Post a list of primary UIDs to the Entrez history server
### Aliases: epost

### ** Examples

## Uploading UIDs to the Entrez history server #############################

id_list <- c(194680922,50978626,28558982,9507199,6678417)
p <- epost(id=id_list, db="protein")
p



cleanEx()
nameEx("esearch")
### * esearch

flush(stderr()); flush(stdout())

### Name: esearch
### Title: Search and retrieve primary UIDs matching a text query
### Aliases: esearch

### ** Examples

## Basic searching #########################################################

# Get th PubMed IDs (PMIDs) for articles about Chlamydia psittaci
# that have been published in 2011.

cpsit <- esearch(term="Chlamydia psittaci and 2011[pdat]", db="pubmed")
cpsit

# Search in PubMed for articles published by the journal PNAS in Volume 97.

pnas <- esearch(term="PNAS[jour] and 97[vol]", db="pubmed")
pnas

# Search for protein UIDs corresponding to BRCA1 in humans.
# To ensure that we retrieve the full list of IDs, we use 
# ecount() to set the 'retmax' parameter.

query <- "BRCA1 and human"
brca1 <- esearch(query, "protein", retmax=ecount(query, "protein"))

## Storing search results on the Entrez history server #####################

query <- "BRCA1 and human"
brca1 <- esearch(query, "protein", retmax=2443, usehistory=TRUE)
brca1



cleanEx()
nameEx("esummary")
### * esummary

flush(stderr()); flush(stdout())

### Name: esummary
### Title: Retrieve document summaries (DocSums)
### Aliases: esummary

### ** Examples

## Downloading Document Summaries ##########################################

# Download DocSums for some protein GIs: 313848131,313847824,313847819,
# 313847818,313847817

gi_list <- c(313848131,313847824,313847819,313847818,313847817)
doc <- esummary(id=gi_list, db="protein")
doc

# Download data from a previous ESearch

(s <- esearch(term="evolution and ecology", db="journals", usehistory=TRUE))
doc <- esummary(s)

data.frame(stringsAsFactors=FALSE,
           uids = names(doc$documentSummary), 
           titles = sapply(doc$documentSummary, "[[", "Title"))

# Get accession numbers for a list of GIs

gi_list <- c(313848131,313847824,313847819,313847818,313847817)
prot <- esummary(gi_list, "protein")
# use docsum() to access the Document Summary information
accn <- docsum(prot)$caption



cleanEx()
nameEx("rentrez-package")
### * rentrez-package

flush(stderr()); flush(stdout())

### Name: rentrez-package
### Title: The rentrez package provides an interface to NCBI's Entrez
###   utilities.
### Aliases: rentrez-package
### Keywords: package

### ** Examples

############################################################################
# combine esearch with esummary/efetch

# Download PubMed records that are indexed in MeSH for both 'Clamydia' and 
# 'genome' and were published in 2011.
query <- "Chlamydia[mesh] and genome[mesh] and 2011[pdat]"

# check how many records fit the bill and download the PMIDs
n <- ecount(query, db="pubmed")
pmids <- esearch(query, db="pubmed", retmax=n)

# Have a quick look at the titles
titles <- docsum(esummary(pmids))$title
titles

# Fetch the abstracts and write them to file
abstracts <- efetch(pmids, rettype="abstract")
write(abstracts, file="~/chlamydia_genome_2011.pmd")

############################################################################
# combine epost with esummary/efetch

# Download protein records corresponding to a list of GI numbers.
id_list <- c(194680922,50978626,28558982,9507199,6678417)

# post the GI numbers to the Entrez history server
p <- epost(id_list, "protein")

# retrieve docsums with esummary
docsums <- docsum(esummary(p))
docsums

# get FASTAs with efetch and write them to file
prot <- efetch(p, rettype="fasta")
write(prot, file="~/prot.fa")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
