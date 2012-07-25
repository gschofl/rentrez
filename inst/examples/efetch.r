## Download full records ###################################################

# Download nucleotide GIs 84785889 and 84785885 in FASTA format
gi_list <- c("84785889","84785885")
f <- efetch(gi_list, db="nucleotide", rettype="fasta")

# Retrieve into a DNAStringSet
fasta <- content(f)
fasta

# Download nucleotide GIs 84785889 and 84785885 in GenBank format
gi_list <- c("84785889","84785885")
f <- efetch(gi_list, "nucleotide")

# Write to file
write(f, file="~/data.gbk")

# or parse as gbRecords
gb <- content(f)
gb


# Download data from pubmed
query <- "Chlamydia psittaci and genome and 2011[pdat]"
(cpsit <- esearch(query, "pubmed", usehistory=TRUE))
publ <- efetch(cpsit)

# retrieve the xml data
publ_xml <- content(publ, parse = FALSE)

# retrieve the parsed date
publ_bib <- content(publ)
