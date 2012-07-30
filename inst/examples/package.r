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
n <- ecount(query, "pubmed")
pmids <- esearch(query, "pubmed", retmax=n)

#
# Fetch the records and parse them to bibentries
#
articles <- efetch(pmids)
parsed_articles <- content(articles)
parsed_articles

#
# look at the abstract
#
abstract(parsed_articles[2])

## Not run
## get the paper
# browse(parsed_articles[2])


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
sum <- content(esummary(p))

#
# get FASTAs with efetch
#
prot <- efetch(p, rettype = "fasta")

#
# retrieve the content of the efetch object
#
seq <- content(prot)

#
# alternatively use DNAbin as a container for the sequence
#
seq <- content(prot, format="DNAbin")

