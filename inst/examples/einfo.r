databases <- einfo()
databases

pubmed <- einfo(db="pubmed")
pubmed$fields[,c("Name", "Description")]
