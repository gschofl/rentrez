## Find related data through ELink #########################################

# Example: find a set of pubmed IDs linked to nucleotide GIs
gis <- c("84785887","84785899","84785907","84785905","84785889")
rv <- elink(gis, "nucleotide", "pubmed")
rv

# Fetch the XML records for the linked articles
efetch(rv)

# Example: Check whether two nucleotide sequences have any LinkOut providers.
gis <- c("84785887","84785899")
rv <- elink(gis, "nucleotide", cmd="lcheck")
rv

# Example: Find publication linked to a protein sequence
a <- esearch("Chlamydia and cpaf", "protein")
a
l <- elink(a[1], dbTo="pubmed")
l
efetch(l)
