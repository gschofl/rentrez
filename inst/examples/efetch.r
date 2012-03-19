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
