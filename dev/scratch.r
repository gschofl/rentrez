loadAll <- function (pkg, document=FALSE) {
  setwd("~/R/Devel/")
  # remove.packages(pkg)
  if (document) document(pkg)
  build(pkg)
  install(pkg)
  do.call(library, list(pkg))
}
loadAll("reutils", document=F)
remove.packages("reutils")
setwd("~/R/Devel/")
document("reutils")

getClassDef("eutil")
help("einfo")

# test esummary ######################################################## {{{
# Construct url, fetch response, construct eutil object
.local.query <- function (eutil, ...) {
  stopifnot(require(XML))
  stopifnot(require(RCurl))
  eutils_host <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/'
  query_string <- reutils:::.query_string(...)
  url <- sprintf('%s%s.fcgi%s', eutils_host, eutil, query_string)
  xml = xmlTreeParse(getURL(url), useInternalNodes = TRUE)
  xml
}

(o <- .local.query(eutil="esearch", db="nucleotide", 
                  term='\"Penaeus monodon[organism]', retstart=4,
                  retmax=6, field=NULL, datetype=NULL,
                  reldate=NULL, mindate=NULL, maxdate=NULL))


dtd <- toString.XMLNode(xmlRoot(o, skip=FALSE))
DTD <- parseDTD(regmatches(dtd, regexpr("http://.*\\.dtd", dtd)))

uids <- esearch('\"Penaeus monodon\"[organism]', db="nucleotide", retmax=2)
uids
efetch(uids[1])
uids@xml
uids@error

s <- esummary(uids[1])
s <- esummary("1234", db="pubmed")
docsums <- s$documentSummary
as.data.frame(do.call(rbind, docsums))

pm <- getUIDs(term='Chip-Seq[TITLE]', db="pubmed")
res100 <- esummary(pm[1:100])
str(res100[1])
res100[1]$PubDate
# }}}

# test einfo ########################################################### {{{
einfo()
listDatabases()
a = einfo()
a
a[1:10]
debug(einfo)

a = einfo("genome")
a$fields$Name
listFields("genome")
listLinks("genome")

# }}}

# test esearch ########################################################## {{{
esearch("Chlamydia[organism]", "genome")
a = esearch("Chlamydiales[organism]", "nucleotide", retmax=1000)
x <- esummary(a[200:300])
l <- sapply(x@documentSummary, function(x) x$Length)
large <- l[which(as.numeric(l) > 1000000)]       
esummary(names(large[1]), "nucleotide")

small <- l[which(as.numeric(l) < 1000000)]
       
       
# }}}

# test efetch ########################################################### {{{
library(reutils)
library(biofiles)
library(genomes)

terms <- c('\"Chlamydophila psittaci\"[organism] OR \"Chlamydia psittaci\"[organism]')
chlamydia <- esearch(terms, "nucleotide", usehistory=TRUE, retmax=10000)
chlamydia
sum <- esummary(chlamydia)
x <- .docsum.sequence(sum)
g <- x[as.numeric(x$Length) > 1000000,]
cpsit <- efetch(g[1,]$GI, "nuccore")
cpsit <- importGenBank(cpsit, dir=tempdir())
cpsit@url

}}}

setwd("~/R/Devel/")
load_all("reutils")

db <- 'pubmed'
query <- 'asthma[mesh]+AND+leukotrienes[mesh]+AND+2009[pdat]'
res <- esearch(query, db, usehistory=TRUE)
web <- res$webEnv
key <- res$queryKey
res <- esummary(db=db, query_key=key, WebEnv=web)
res2 <- efetch(db=db, query_key=key, WebEnv=web, retmode="xml")


db = 'protein'
id_list = '194680922,50978626,28558982,9507199,6678417'
output = esummary(id_list, db)
data <- efetch(id_list, db, rettype="fasta", retmode="text")
cat(data@data)

output@documentSummary$"194680922"
ds <- output@documentSummary

res <- esearch(db="nuccore", retmax=10, term="opuntia[ORGN] accD")
sum <- esummary(res)

.docsum(sum)


.docsum(output)




                       
        

  
  
  
  
}

















# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

