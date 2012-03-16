# Search in PubMed with the term cancer for abstracts that have an Entrez
# date within the last 60 days and retrieve the first 100 PMIDs.

cancer <- esearch(term="cancer", db="pubmed", reldate=60, datetype='edat')
cancer

# Search in PubMed for the journal PNAS, Volume 97.

pnas <- esearch(term="PNAS[jour] and 97[vol]", db="pubmed")
pnas

# Search for protein UIDs corresponding to BRCA1 in humans.
# To ensure that we retrieve the full list of IDs, we use 
# ecount() to set the 'retmax' parameter.

query <- "BRCA1 and human"
brca1 <- esearch(query, db="protein", retmax=ecount(query, "protein"))
brca1

# Search in the Entrez Nucleotide database for all tRNAs and post the
# results on the Entrez history server and return a WebEnv and query_key.

trna <- esearch("biomol trna[prop]", "nucleotide", usehistory=TRUE)
trna

# Search in the Entrez Protein database for molecules within a molecular
# weight range and return the first 100 UIDs.

prot <- esearch("80000:90000[molecular weight]", "protein")
prot
