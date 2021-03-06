% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeSimilarity.R
\name{computeSimilarity}
\alias{computeSimilarity}
\title{Compute Spectral Similarity}
\usage{
computeSimilarity(
  mzDt,
  ppmTol,
  isCentroid,
  intensityMin = 0,
  normalize = TRUE,
  normalization_method = "sqrt",
  similarity_method
)
}
\arguments{
\item{mzDt}{a data.table imported by mzML2dataTable() with columns `seqNum`, `mz`, `intensity`}

\item{ppmTol}{the mass accuracy of the instrument}

\item{isCentroid}{Logical. Is the data centered?}

\item{intensityMin}{Optional parameter. Ions with intensity < intensityMin will be removed. Default = 0 allowing all ions into algorighm.}

\item{normalize}{Optional parameter. Should the intensity be normalized before any processing is done? Default = TRUE.}

\item{normalization_method}{Optional parameter. Select normalization method: "maxPeak", "sqrt", "sum". See `.normalizeEachSpectrum_dt` for details.}

\item{similarity_method}{Select similarity metric to be used. "dotProd", "specContrast", "specCor", "brayCurtis", "euclidean".}
}
\value{
a 3 column matrix containing the similarity score and seqNum of each compared spectrum
}
\description{
Compute Spectral Similarity
}
\examples{
dt <- mzML2dataTable(path = msdata::proteomics(full.names = TRUE)[3])
simMat <- computeSimilarity(mzDt = dt[msLevel == 2 & seqNum \%between\% c(1, 10)], 
                            ppmTol = 100, 
                            isCentroid = TRUE, 
                            intensityMin = 0.01, 
                            normalize = TRUE, 
                            normalization_method = "sqrt", 
                            similarity_method = "specContrast")

squareMat <- similarity2SquareMatrix(simMat)
image(squareMat)

}
