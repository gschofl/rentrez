# Download nucleotide GIs 84785889 and 84785885 in FASTA format:
gis <- c("84785889","84785885")
a <- efetch(gis, "nucleotide", rettype="fasta")

# Retrieve the downloaded record as an AAStringSet:
seq <- content(a)

# or turn it into a 'DNAbin' object:
seq <- content(a, format = "DNAbin")

# or as a (list of) character vectors
seq <- content(a, format = "String")

# or as it is returned from NCBI
seq <- content(a, parse = FALSE)

# Alternatively use accession numbers:
acc_no <- "AAA23146"
b <- efetch(acc_no, "protein", rettype="fasta")

# Download nucleotide GIs 84785889 and 84785885 in GenBank format
gis <- c("84785889","84785885")
c <- efetch(gis, "nucleotide")

# Write to file
write(c, file="~/data.gbk")

# or parse as gbRecords
gb <- content(c)
gb

# Download data from pubmed
query <- "Chlamydia psittaci and genome and 2012[pdat]"
cpsit <- esearch(query, "pubmed", usehistory=TRUE)
publ <- efetch(cpsit)

# retrieve the xml data
publ_xml <- content(publ, parse = FALSE)

# retrieve the parsed date
publ_bib <- content(publ)
publ_bib

# have a look at an abstract
abstract(publ_bib[2])

# Not run:
# download the paper
# browse(publ_bib[2])


