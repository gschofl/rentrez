\name{einfoDb-class}
\alias{einfoDb-class}
\title{einfoDb class}
\description{
  einfoDb is an S4 class that extends the
  \code{\link{einfo-class}}. This class provides a
  container for data retrived by calls to the NCBI EInfo
  utility.
}
\details{
  einfoDb objects have seven slots in addition to the slots
  provided by basic \code{\link{eutil-class}} objects:
  \describe{ \item{dbName}{Name of the target database}
  \item{menuName}{Name of the target database}
  \item{descriptiom}{Short description of the target
  database} \item{records}{Count of records in the target
  database} \item{lastUpdate}{Last update of the target
  database} \item{fields}{Field names of the target
  database} \item{links}{Available links for the target
  database} }
}
\seealso{
  \code{\link{einfo}} for generating calls to the NCBI
  EInfo utility.
}

