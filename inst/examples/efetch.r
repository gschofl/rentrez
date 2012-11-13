# Search the protein database for Chlamydia CPAF:
cpaf <- esearch("Chlamydia[organism] and CPAF", "protein")
cpaf

# Fetch the fasta sequence of the first 5 hits:
cpaf_fasta <- efetch(cpaf[1:5], rettype="fasta")
cpaf_fasta

# Directly download sequences using GIs:
gis <- c("84785889","84785885")
a <- efetch(gis, "nucleotide", rettype="fasta")

# Retrieve the downloaded record as text string:
seq <- content(a)

# Alternatively use accession numbers:
acc_no <- "AAA23146"
b <- efetch(acc_no, "protein", rettype="fasta")

# Download nucleotide GIs 84785889 and 84785885 in GenBank format (default):
gis <- c("84785889","84785885")
c <- efetch(gis, "nucleotide")

# Write to file
write(c, file="~/data.gbk")

# Download data from pubmed
query <- "Chlamydia psittaci and genome and 2012[pdat]"
cpsit <- esearch(query, "pubmed", usehistory=TRUE)
publ <- efetch(cpsit)

# retrieve the xml data
publ_xml <- content(publ)
