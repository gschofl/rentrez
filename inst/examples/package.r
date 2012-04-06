############################################################################
# combine esearch with esummary/efetch

# Download PubMed records that are indexed in MeSH for both 'Clamydia' and 
# 'genome' and were published in 2011.
query <- "Chlamydia[mesh] and genome[mesh] and 2011[pdat]"

# check how many records fit the bill and download the PMIDs
n <- ecount(query, db="pubmed")
pmids <- esearch(query, db="pubmed", retmax=n)

# Have a quick look at the titles
# doc <- esummary(pmids)

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
# docsums <- docsum(esummary(p))
# docsums

# get FASTAs with efetch and write them to file
prot <- efetch(p, rettype="fasta")
write(prot, file="~/prot.fa")
