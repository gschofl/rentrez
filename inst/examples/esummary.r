# View Document Summaries from PubMed

doc <- esummary(id=c(22382892,22302240,22299031,22296995), db="pubmed")
doc

# View Document Summaries from Journals

doc <- esummary("37381", "journals")
doc

# Combine a esearch with esummary

doc <- esummary(esearch(term="Evolution", db="journals", retmax=10))
sapply(doc$documentSummary, "[", "Title")

# View Document Summaries from Gene

esummary(c(828392,790,470338), "gene")

# Get accession numbers for a list of GIs

prot <- esummary(c(403164,45447012,27806117), "protein")
prot
# use docsum() to transform the Document Summary into a data frame
accn <- docsum(prot)$caption
