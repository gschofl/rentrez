## Downloading Document Summaries ##########################################

# Download DocSums for some protein GIs: 313848131,313847824,313847819,
# 313847818,313847817

gi_list <- c(313848131,313847824,313847819,313847818,313847817)
doc <- esummary(id=gi_list, db="protein")
doc

# Download data from a previous ESearch.
(s <- esearch(term="evolution and ecology", db="journals", usehistory=TRUE))
doc <- esummary(s)

# Retrieve the contents of the esearch object as a data frame.
doc <- content(doc)
doc["Title"]

# Get accession numbers for a list of GIs
gi_list <- c(313848131,313847824,313847819,313847818,313847817)
prot <- content(esummary(gi_list, "protein"))
names(prot)
prot[,c("Id","Caption","Length","TaxId")]
