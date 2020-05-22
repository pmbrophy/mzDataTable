#' Index the m/z axis
#'
#' @description Index the m/z axis for a set of mass spectra. Function outputs
#'   normalized m/z values and integer index values.
#'
#' @param mz a data.table containing a column named mz
#' @param isCentroid logical, default is `TRUE`. Are the spectra centroids?
#'
#' @return Returns the original data.table also containing the the normalized
#'   m/z values and their index
#' @export
#'
#' @examples
#' mz <- c(100, 100.01, 100.02, 100.021, 100.2, 100.21, 100.22, 100.22, 100.31)

indexMasterSpectrum <- function(dt, ppmTol, isCentroid = TRUE){
  #Input checks
  notDataTable <- !data.table::is.data.table(dt)
  mzColMissing <- !("mz" %in% colnames(dt))
  if(notDataTable){
    stop("dt is not a data.table")
  }else if(mzColMissing){
    stop("dt does not contain a column named 'mz'")
  }

  if(isCentroid){
    dt_grid <- .C_normCentroidMz(mz = dt$mz, ppmTol = ppmTol)

  }else{
    dt_grid <- .normProfileMz(mz = dt$mz)
  }
  #Return result
  data.table::data.table(dt, dt_grid)
}

#' DEPRICATED: Calculate global m/z grid for centroids
#'
#' @param mz a vector of m/z values to be clustered
#' @param ppmTol tolerance for cluster width in parts per million
#'
#' @return a data.table
#'
#' @examples
#' \dontrun{
#' mzs <- c(100.001, 100.002, 100.003, 100.01, 100.011, 100.012, 100.03)
#' dt <- normCentroidMz(mz = mzs, ppmTol = 10)
#' }
#'

.normCentroidMz <- function(mz, ppmTol){
  if(!is.numeric(mz)){
    stop("mz is not numeric")
  }

  mz_length <- length(mz)
  index <- c(1:mz_length)
  tol <- ppmTol*(10^(-6))

  #Calculate all uppper limits assuming each mz is unique mzGrid
  mz_upperLimit <- mz*tol + mz

  #index and sort
  mz <- data.table(mz = mz, index = index, mz_upperLimit = mz_upperLimit)
  data.table::setkey(x = mz, "mz", physical = TRUE)

  #output results
  mzGrid <- vector(mode = "numeric", length = mz_length)
  mzGrid_index <- vector(mode = "numeric", length = mz_length)
  mzGrid_upperLimit <- vector(mode = "numeric", length = mz_length)

  #initialize index 1
  mzGrid[1] <- mz$mz[1]
  mzGrid_index[1] <- 1
  mzGrid_upperLimit[1] <- mz$mz_upperLimit[1]

  for (i in c(2:mz_length)) {
    #previous location
    j <- i-1

    mz_in_range <- mz$mz[i] <= mzGrid_upperLimit[j]
    if(mz_in_range){
      #Same mz grid
      #assign previous mzGrid value to current
      mzGrid[i] <- mzGrid[j]

      #assign previous mzGrid_index to current
      mzGrid_index[i] <- mzGrid_index[j]

      #assign previous mz_upperLimit to current
      mzGrid_upperLimit[i] <- mzGrid_upperLimit[j]
    }else{
      #New mz grid
      #assign current mzGrid value to current mz
      mzGrid[i] <- mz$mz[i]

      #assign current mzGrid_index to
      mzGrid_index[i] <- mzGrid_index[j] + 1

      #assign previous mz_upperLimit to current
      mzGrid_upperLimit[i] <- mz$mz_upperLimit[i]
    }
  }
  #Return
  mz[, mz_upperLimit := NULL]
  mz[, c("mzGrid", "mzGrid_index", "mzGrid_upperLimit") := .(mzGrid,  mzGrid_index, mzGrid_upperLimit)]
  data.table::setkey(x = mz, "index", physical = TRUE)
  mz[, index := NULL]
  mz
}

#' Calculate global m/z grid for profile mode data
#'
#' @param mz a vector of m/z values to be clustered
#'
#' @return a data.table
#'

.normProfileMz <- function(mz){
  if(!is.numeric(mz)){
    stop("mz is not numeric")
  }
  #Construct a data.table of unique mz values and index
  mz <- data.table::data.table(mz = mz, index = c(1:length(mz)))
  mzGrid <- Rfast::sort_unique(x = mz$mz)
  mzGrid_index <- c(1:length(mzGrid))

  #data.table results
  mzGrid <- data.table::data.table(mz = mzGrid, mzGrid_index = mzGrid_index)

  #Combine Results
  mz <- data.table::merge.data.table(x = mz, y = mzGrid, by = "mz")

  #reorder to orignal input
  data.table::setkey(x = mz, "index", physical = TRUE)
.
  #Remove index
  index <- NULL
  mz[, index := NULL]
  mz
}




#' C++ Version: Calculate global m/z grid for centroids
#'
#' @param mz a vector of m/z values to be clustered
#' @param ppmTol tolerance for cluster width in parts per million
#'
#' @return a data.table
#'
#' @examples
#' \dontrun{
#' mzs <- c(100.001, 100.002, 100.003, 100.01, 100.011, 100.012, 100.03)
#' dt <- normCentroidMz(mz = mzs, ppmTol = 10)
#' }
#'

.C_normCentroidMz <- function(mz, ppmTol){
  if(!is.numeric(mz)){
    stop("mz is not numeric")
  }

  mz_length <- length(mz)
  index <- c(1:mz_length)
  tol <- ppmTol*(10^(-6))

  #Calculate all uppper limits assuming each mz is unique mzGrid
  mz_upperLimit <- mz*tol + mz

  #index and sort
  mz <- data.table(mz = mz, index = index, mz_upperLimit = mz_upperLimit)
  data.table::setkey(x = mz, "mz", physical = TRUE)

  #Call to Rcpp
  mz_grid <- C_normCentroidMz(mz_vector = mz$mz, mz_upperLimit_vector = mz$mz_upperLimit)

  #Return
  #mz[, mz_upperLimit := NULL]
  #mz[, c("mzGrid", "mzGrid_index", "mzGrid_upperLimit") := .(mz_grid[[1]],  mz_grid[[2]], mz_grid[[3]])]
  mz <- data.table(mz = mz$mz, index = mz$index, mzGrid = mz_grid[[1]], mzGrid_index = mz_grid[[2]])

  data.table::setkey(x = mz, "index", physical = TRUE)

  #mz[, index := NULL]
  mz
}