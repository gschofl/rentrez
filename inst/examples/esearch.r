## Basic searching #########################################################

# Get th PubMed IDs (PMIDs) for articles about Chlamydia psittaci
# that have been published in 2011.

cpsit <- esearch(term="Chlamydia psittaci and 2011[pdat]", db="pubmed")
cpsit

# Search in PubMed for articles published by the journal PNAS in Volume 97.

pnas <- esearch(term="PNAS[jour] and 97[vol]", db="pubmed")
pnas

# Search for protein UIDs corresponding to BRCA1 in humans.
# To ensure that we retrieve the full list of IDs, we count
# the number of hits to set the 'retmax' parameter.

query <- "BRCA1 and human"
n <- count(esearch(query, "protein", rettype="count"))
brca1 <- esearch(query, "protein", retmax=n)

## Storing search results on the Entrez history server #####################

query <- "BRCA1 and human"
brca1 <- esearch(query, "protein", retmax=2443, usehistory=TRUE)
brca1
