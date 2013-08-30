## Downloading Document Summaries ##########################################

# Download DocSums for some protein GIs: 313848131,313847824,313847819,
# 313847818,313847817

id <- c(313848131,313847824,313847819,313847818,313847817)
doc <- esummary(id, "protein")

# Download data from a previous ESearch.
s <- esearch(term="evolution and ecology", db="journals", usehistory=TRUE)
doc <- esummary(s)

# Retrieve the docsum as a data frame or as the XML response.
ds <- docsum(doc)
ds[["Title"]]

ds_xml <- content(doc, "xml")

# Get accession numbers for a list of GIs
id <- c(313848131, 313847824, 313847819, 313847818, 313847817)
prot <- docsum(esummary(id, "protein"))
names(prot)
prot[, c("Id","Caption","Slen","TaxId")]
