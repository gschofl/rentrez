## Download a large set of records (>500)

# Download all human ncRNAs in FASTA format
query <- "human[orgn] and biomol ncrna[prop]"

# Let's check how many there are
esearch(query, "nucleotide", rettype="count")

# upload the GIs to the history server
ncrna <- esearch(query, "nucleotide", usehistory=TRUE)
ncrna

## Not run
## download ncRNAs in batch mode
# fasta <- efetch.batch(id=ncrna, chunk_size=200, rettype="fasta")
# write(fasta, "~/human_small_nuclear_rna.fasta")
