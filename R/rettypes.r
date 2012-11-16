#' @keywords internal
#' @export
set_record_type <- function(db, rettype = NULL, retmode = NULL) {
  rt <- set_rettype(db, rettype)
  rm <- set_retmode(db, rt, retmode)
  list(rettype = rt, retmode = rm)
}

# not NULL
"%nn%" <- function (a, b) {
  if (is.null(a)) a else b
}

set_rettype <- function (db, rt = NULL) {
  db <- switch(db, nucleotide = 'nuccore', db)
  switch(db,
         biosample = match.arg(rt, "full"),
         biosystems = match.arg(rt, "xml"),
         gds = match.arg(rt, "summary"),
         gene = rt %nn% match.arg(rt, "gene_table"),
         homologene = rt %nn% match.arg(rt, c("alignmentscores", "fasta",
                                              "homologene")),
         mesh = match.arg(rt, "full"),
         nlmcatalog = NULL,
         nuccore = rt %nn% match.arg(rt, c("fasta", "gb", "gbc", "ft",
                                           "gbwithparts", "fasta_cds_na",
                                           "fasta_cds_aa", "seqid", "acc",
                                           "native")),
         nucest = rt %nn% match.arg(rt, c("fasta", "gb", "gbc",
                                          "seqid", "acc", "native", "est")),
         nucgss = rt %nn% match.arg(rt, c("fasta", "gb", "gbc",
                                          "seqid", "acc", "native", "gss")),
         popset = rt %nn% match.arg(rt, c("fasta", "gb", "gbc", "seqid",
                                          "acc", "native")),
         protein = rt %nn% match.arg(rt, c("fasta", "gp", "gpc", "ft",
                                           "seqid", "acc", "native")),
         pmc = rt %nn% match.arg(rt, "medline"),
         pubmed = rt %nn% match.arg(rt, c("medline", "uilist", "abstract")),
         snp = rt %nn% match.arg(rt, c("flt", "fasta", "rsr", "ssexemplar",
                                       "chr", "genxml", "docset", "uilist")),
         sra = match.arg(rt, "full"),
         taxonomy = rt %nn% match.arg(rt, "uilist"),
         'Database not supported')
}


set_retmode <- function (db, rt, rm = NULL) {
  db <- switch(db, nucleotide = 'nuccore', db)
  switch(db,
         biosample = switch(rt,
                             full = match.arg(rm, c("xml", "text"))
         ),
         biosystems = switch(rt,
                             xml = match.arg(rm, c("xml"))
         ),
         gds = switch(rt,
                      summary = match.arg(rm, c("text"))
         ),
         gene = switch(rt %||% 'null',
                       null = match.arg(rm, c("xml", "asn.1")),
                       gene_table = match.arg(rm, c("text"))
         ),
         homologene = switch(rt %||% 'null',
                             null = match.arg(rm, c("xml", "asn.1")),
                             alignmentscores = match.arg(rm, c("text")),
                             fasta = match.arg(rm, c("text")),
                             homologen = match.arg(rm, c("text"))
         ),
         mesh = switch(rt,
                       full = match.arg(rm, c("text"))
         ),
         nlmcatalog = switch(rt %||% 'null',
                             null = match.arg(rm, c("xml", "text"))
         ),
         nuccore = switch(rt %||% 'null',
                          null = match.arg(rm, c("text")),
                          native = match.arg(rm, c("xml")),
                          acc = match.arg(rm, c("text")),
                          fasta = match.arg(rm, c("xml", "text")),
                          seqid = match.arg(rm, c("text")),
                          gb = match.arg(rm, c("text", "xml")),
                          gbc = match.arg(rm, c("xml")),
                          ft = match.arg(rm, c("text")),
                          gbwithparts = match.arg(rm, c("text")),
                          fasta_cds_na = match.arg(rm, c("text")),
                          fasta_cds_aa = match.arg(rm, c("text"))
                          
         ),
         nucest = switch(rt %||% 'null',
                         null = match.arg(rm, c("text")),
                         native = match.arg(rm, c("xml")),
                         acc = match.arg(rm, c("text")),
                         fasta = match.arg(rm, c("xml", "text")),
                         seqid = match.arg(rm, c("text")),
                         gb = match.arg(rm, c("text", "xml")),
                         gbc = match.arg(rm, c("xml")),
                         est = match.arg(rm, c("xml"))
                         
         ),
         nucgss = switch(rt %||% 'null',
                         null = match.arg(rm, c("text")),
                         native = match.arg(rm, c("xml")),
                         acc = match.arg(rm, c("text")),
                         fasta = match.arg(rm, c("xml", "text")),
                         seqid = match.arg(rm, c("text")),
                         gb = match.arg(rm, c("text", "xml")),
                         gbc = match.arg(rm, c("xml")),
                         gss = match.arg(rm, c("xml"))
                         
         ),
         protein = switch(rt %||% 'null',
                          null = match.arg(rm, c("text")),
                          native = match.arg(rm, c("xml")),
                          acc = match.arg(rm, c("text")),
                          fasta = match.arg(rm, c("xml", "text")),
                          seqid = match.arg(rm, c("text")),
                          gp = match.arg(rm, c("text", "xml")),
                          gpc = match.arg(rm, c("xml")),
                          ft = match.arg(rm, c("text"))
                          
         ),
         popset = switch(rt %||% 'null',
                         null = match.arg(rm, c("text")),
                         native = match.arg(rm, c("xml")),
                         acc = match.arg(rm, c("text")),
                         fasta = match.arg(rm, c("xml", "text")),
                         seqid = match.arg(rm, c("text")),
                         gb = match.arg(rm, c("text", "xml")),
                         gbc = match.arg(rm, c("xml"))
                         
         ),
         pmc = switch(rt %||% 'null',
                      null = match.arg(rm, c("xml")),
                      medline = match.arg(rm, c("text"))
         ),
         pubmed = switch(rt %||% 'null',
                         null = match.arg(rm, c("xml", "asn.1")),
                         medline = match.arg(rm, c("text")),
                         uilist = match.arg(rm, c("text")),
                         abstract = match.arg(rm, c("text"))               
         ),
         snp = switch(rt %||% 'null',
                      null = match.arg(rm, c("xml", "asn.1")),
                      flt = match.arg(rm, c("text")),
                      fasta = match.arg(rm, c("text")),
                      rsr = match.arg(rm, c("text")),
                      ssexemplar = match.arg(rm, c("text")),
                      chr = match.arg(rm, c("text")),
                      genxml = match.arg(rm, c("xml")),
                      docset = match.arg(rm, c("text")),
                      uilist = match.arg(rm, c("xml", "text"))  
         ),
         sra = switch(rt,
                      full = match.arg(rm, c("xml"))
         ),
         taxonomy = switch(rt %||% 'null',
                           null = match.arg(rm, c("xml")),
                           uilist = match.arg(rm, c("xml", "text"))
         )
  )
}
