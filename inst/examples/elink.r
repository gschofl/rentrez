## Find related data through ELink #########################################

# find a set of pubmed IDs linked to nucleotide GIs
gi_list <- c("84785887","84785899","84785907","84785905","84785889")
links <- elink(id=gi_list, dbFrom="nucleotide", dbTo="pubmed")
links

esummary(links)
