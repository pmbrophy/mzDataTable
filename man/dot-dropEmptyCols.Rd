% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mzML2_OtherFormat_Internals.R
\name{.dropEmptyCols}
\alias{.dropEmptyCols}
\title{Drop empty columns from data.frame}
\usage{
.dropEmptyCols(df)
}
\arguments{
\item{df}{a data frame}
}
\value{
returns a data.frame
}
\description{
Check for any columns in a data frame that contain only na and
remove
}
\examples{
\dontrun{
df <- data.frame(c1 = c(1:10),
                 c2 = rnorm(n = 10, mean = 1, sd = 1),
                 c3 = rep("a", 10),
                 c4 = rep(NA, 10),
                 c5 = rep(NaN, 10),
                 C6 = c(1:10))

ncol(df)
df_out <- .dropEmptyCols(df = df)
ncol(df_out)
}

}
