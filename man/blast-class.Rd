\name{blast-class}
\alias{blast-class}
\alias{blast,blast-method}
\alias{show,blast-method}
\title{blast class}
\usage{
  .blast(...)
}
\description{
  blast is an S4 class that provides a container for data
  retrived by calls to the NCBI Blast utility.
}
\details{
  blast objects have nine slots: \describe{
  \item{bitscore}{} \item{evalue}{} \item{mlog.evalue}{}
  \item{gi}{} \item{accession}{} \item{query}{}
  \item{params}{} \item{hit_table}{} \item{hsp_list}{} }
}

