% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getBPI.R
\name{getBPI}
\alias{getBPI}
\title{Get Base Peak Intensity}
\usage{
getBPI(mzObj, normalize = FALSE, method = "sqrt")
}
\arguments{
\item{mzObj}{a data.table or disk.frame imported from mzML/mzXML file.}

\item{normalize}{Logical. Default = FALSE. Should the data be normalized?}

\item{method}{the normalization method. "maxPeak", "sum", or "sqrt".}
}
\value{
Returns a data.table
}
\description{
Get Base Peak Intensity
}
\examples{

}
