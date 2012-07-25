## get database statistics and search fields

# a list of all valid Entrez databases
databases <- einfo()
databases

# database statistics for protein
protein <- einfo(db="protein")
protein

# search fields for protein
p <- content(protein)
cbind(p$fields$Name, strtrim(p$fields$Description, 60))
