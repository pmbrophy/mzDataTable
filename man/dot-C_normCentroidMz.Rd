% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IndexMasterSpectrum.R
\name{.C_normCentroidMz}
\alias{.C_normCentroidMz}
\title{C++ Version: Calculate global m/z grid for centroids}
\usage{
.C_normCentroidMz(mz, ppmTol)
}
\arguments{
\item{mz}{a vector of m/z values to be clustered}

\item{ppmTol}{tolerance for cluster width in parts per million}
}
\value{
a data.table
}
\description{
C++ Version: Calculate global m/z grid for centroids
}
\examples{
\dontrun{
mzs <- c(100.001, 100.002, 100.003, 100.01, 100.011, 100.012, 100.03)
dt <- normCentroidMz(mz = mzs, ppmTol = 10)
}

}
