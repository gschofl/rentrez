## get database statistics and search fields

# a list of all valid Entrez databases
databases <- einfo()
databases

# database statistics for protein
protein <- einfo("protein")
protein

# retrieve xml response
p <- content(protein, "xml")
p
