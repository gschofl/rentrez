#
# combine esearch with efetch
#
# Download PubMed records that are indexed in MeSH for both 'Clamydia' and 
# 'genome' and were published in 2012.
#
query <- "Chlamydia[mesh] and genome[mesh] and 2012[pdat]"

#
# check how many records fit the bill and download the PMIDs
#
n <- esearch(query, "pubmed", rettype="count")
n
pmids <- esearch(query, "pubmed", retmax=count(n))

#
# Fetch the records and retrieve the XML response
#
articles <- efetch(pmids)
articles_xml <- content(articles)
articles_xml


############################################################################
#
# combine epost with esummary/efetch
#
# Download protein records corresponding to a list of GI numbers.
#
id <- c(194680922,50978626,28558982,9507199,6678417)

#
# post the GI numbers to the Entrez history server
#
p <- epost(id, "protein")

#
# retrieve docsums with esummary
#
sum <- docsum(esummary(p))

#
# get FASTAs with efetch
#
prot <- efetch(p, retmode="text", rettype="fasta")

#
# retrieve the content of the efetch object
#
seq <- content(prot)

