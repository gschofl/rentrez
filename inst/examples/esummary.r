## Downloading Document Summaries ##########################################

# Download DocSums for some protein GIs: 313848131,313847824,313847819,
# 313847818,313847817

gi_list <- c(313848131,313847824,313847819,313847818,313847817)
doc <- esummary(id=gi_list, db="protein")
doc

# Download data from a previous ESearch

(s <- esearch(term="evolution and ecology", db="journals", usehistory=TRUE))
doc <- esummary(s)

data.frame(stringsAsFactors=FALSE,
           uids = names(doc$documentSummary), 
           titles = sapply(doc$documentSummary, "[[", "Title"))

# Get accession numbers for a list of GIs

gi_list <- c(313848131,313847824,313847819,313847818,313847817)
prot <- esummary(gi_list, "protein")
# use docsum() to access the Document Summary information
accn <- docsum(prot)$caption
