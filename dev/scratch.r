loadAll <- function (pkg, document=FALSE) {
  setwd("~/R/Devel/")
  remove.packages(pkg)
  if (document) document(pkg)
  build(pkg)
  install(pkg)
  do.call(library, list(pkg))
}
loadAll("reutils", document=TRUE)
remove.packages("reutils")
getClassDef("eutil")
help("einfo")

# test esummary ######################################################## {{{
uids <- esearch('\"Penaeus monodon\"[organism]', db = "nucleotide")
s <- esummary(uids[1:10])
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
a = einfo("genome")
a
a$fields$Name
listFields("genome")
listLinks("genome")

# }}}

# test esearch ########################################################## {{{
esearch("Chlamydia[organism]", "genome")
a = esearch("Chlamydia[organism]", "genome")
a
length(a)
a[1:3]
length(a[1:3])

years <- 2000:2011
count <- sapply(years, function (y)
  getCount(paste('"ChIP-Seq"[WORD] and', y, "[PDAT]"),
           silent = TRUE))
plot(x=years, y=count, col="red", type="l", lwd=2, ylim=c(0, max(count)))

id <- getUIDs('\"Chlamydia\"[organism]', db = "genome")
id
length(id)
length(id[1:2])

# }}}

# --R-- vim:ft=r:sw=2:sts=2:ts=4:tw=76:
#       vim:fdm=marker:fmr={{{,}}}:fdl=0

