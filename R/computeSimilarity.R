#' Generate square similarity matrix
#'
#' @param simMat the matrix returned by computeSimilarity() or .specSim() containing columns "sim", "seqNum1", "seqNum2"
#'
#' @return Returns a square matrix
#' @export
#'
#' @examples
#' #See computeSimilarity

similarity2SquareMatrix <- function(simMat){
  seqNums <- unique(c(simMat[,"seqNum1"], simMat[,"seqNum2"])) #seqNum index for each col
  dimSize <- length(seqNums)    #num cols
  
  #Square matrix (dimSize x dimSize) to store results
  squareMat <- matrix(data = 0, nrow = dimSize, ncol = dimSize)
  
  #Convert seqNum to index to populate squareMat
  rows <- match(x = simMat[,"seqNum1"], table = seqNums)
  cols <- match(x = simMat[,"seqNum2"], table = seqNums)
  result_loc <- cbind(rows, cols)
  
  
  #Fill squareMat with similarity score values from simMat using result_loc indexs
  squareMat[result_loc] <- simMat[, "sim"]
  
  #Fill in diagonal 
  diag(x = squareMat) <- 1
  
  squareMat
}

#' Compute Spectral Similarity
#'
#' @param mzDt a data.table imported by mzML2dataTable() with columns `seqNum`, `mz`, `intensity`
#' @param ppmTol the mass accuracy of the instrument
#' @param isCentroid Logical. Is the data centered? 
#' @param intensityMin Optional parameter. Ions with intensity < intensityMin will be removed. Default = 0 allowing all ions into algorighm. 
#' @param normalize Optional parameter. Should the intensity be normalized before any processing is done? Default = TRUE. 
#' @param normalization_method Optional parameter. Select normalization method: "maxPeak", "sqrt", "sum". See `.normalizeEachSpectrum_dt` for details. 
#' @param similarity_method Select similarity metric to be used. "dotProd", "specContrast", "specCor", "brayCurtis", "euclidean". 
#'
#' @return a 3 column matrix containing the similarity score and seqNum of each compared spectrum
#' @export
#'
#' @examples
#' dt <- mzML2dataTable(path = msdata::proteomics(full.names = TRUE)[3])
#' simMat <- computeSimilarity(mzDt = dt[msLevel == 2 & seqNum %between% c(1, 10)], 
#'                             ppmTol = 100, 
#'                             isCentroid = TRUE, 
#'                             intensityMin = 0.01, 
#'                             normalize = TRUE, 
#'                             normalization_method = "sqrt", 
#'                             similarity_method = "specContrast")
#' 
#' squareMat <- similarity2SquareMatrix(simMat)
#' image(squareMat)
#' 

computeSimilarity <- function(mzDt, ppmTol, isCentroid, intensityMin = 0, normalize = TRUE, normalization_method = "sqrt", similarity_method){
  #Subset and filter
  mzDt <- mzDt[intensity >= intensityMin, list(seqNum, mz, intensity)]
  
  #Normalize intensity
  if(normalize){
    normIntensity <- .normalizeEachSpectrum_dt(mzDt = mzDt, method = normalization_method)
    
    #Replace summed intensity with normalized summed intensity
    mzDt[, intensity := NULL]
    data.table::setnames(x = mzDt, old = "intensity_norm", new = "intensity")
  }
  
  #Grid all spectra onto a single grid
  mzDt <- indexMasterSpectrum(mzDt = mzDt, 
                              ppmTol = ppmTol, 
                              isCentroid = isCentroid) 
  
  similarity <- .specSim(mzDt = mzDt, similarity_method = similarity_method)
  
  similarity
}


#' @title Calculate spectral similarity
#'
#' @description Calculate the spectral similarity using one of the available
#'   methods. The mzDt containing n-unique seqNums is converted into a matrix
#'   with n-columns. All unique combinations are calculated and returned. The
#'   output can be further processed to a similarity matrix using
#'   similarity2SquareMatrix(). mzDt should contain self-normalized spectra
#'   (see: .normalizeEachSpectrum_dt()). Each spectrum should contain mzGrid and
#'   mzGrid_index corresponding to the global mz grid (see:
#'   indexMasterSpectrum()).
#'
#' @param mzDt the data.table containing two or more spectra to be compared.
#' @param similarity_method choose one: "dotProd", "specContrast", "specCor",
#'   "brayCurtis", "euclidean"
#'
#' @return returns a 3-column matrix containing the similarity score and
#'   sequence numbers
#'   

.specSim <- function(mzDt, similarity_method){
  if(!(similarity_method %in% c("dotProd", "specContrast", "specCor", "brayCurtis", "euclidean"))){
    stop("Similarity method not implemented choose from: \"dotProd\", \"specContrast\", \"specCor\", \"brayCurtis\", \"euclidean\"")
  }
  
  seqNums <- unique(mzDt$seqNum) #seqNum index for each col
  ncols <- length(seqNums)    #num cols
  colIndexs <- c(1L:ncols)  #cols index
  
  #produce matrix: cols = seqNum
  m <- .grid_matrix(mzDt = mzDt, seqNums = seqNums)
  
  #Get column combinations
  colCombos <- combn(x = colIndexs, m = 2, simplify = TRUE) #unique combos by matrix
  nCombos <- ncol(colCombos)
  comboIndexs <- c(1L:nCombos)
  
  #Matrix to store data matrix(sim, seqNum1, seqNum2)
  sims <- matrix(data = 0, 
                 nrow = nCombos, 
                 ncol = 3, 
                 dimnames = list(NULL, 
                                 c("sim", "seqNum1", "seqNum2")))
  
  #Pre-allocate matrix for results and results locations
  result_loc <- cbind(rep(1, times = 3), c(1L:3))
  result <- vector(mode = "numeric", length = 3)
  
  #Calculate similarity: "dotProd", "specContrast", "specCor", "brayCurtis", "euclidean". 
  if(similarity_method == "dotProd"){
    
    ##Dot Product Similarity [0,1]
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- m[,col1] %*% m[,col2]
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }else if(similarity_method == "specContrast"){
    
    ##Spectral Contrast Angle [0,1]
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- 1 - ((2*acos(m[,col1] %*% m[,col2]))/pi)
      
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }else if(similarity_method == "specCor"){
    
    ##Spectral correlation - pearson's r [-1,1]
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- cor(x = m[,col1], y = m[,col2], method = "pearson")
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }else if(similarity_method == "brayCurtis"){
    
    ##Bray-Curtis Distance [0,1]
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- 1 - (sum(abs(m[,col1] - m[,col2])) / sum(m[,col1] + m[,col2]))
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }else if(similarity_method == "euclidean"){
    
    ##Euclidean Distance
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- 1 - sqrt(sum((m[,col1] - m[,col2])^2))
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }
  
  #Return Matrix
  sims
}

#' @title Grid spectra to equal lengths
#'
#' @description After global mz gridding of n-spectra with
#'   indexMasterSpectrum(), fill matrix containing n-columns to generate
#'   comparable spectra.
#'
#' @param mzDt A data.table minimally containing "intensity" and "mzGrid_index"
#' @param seqNums Original spectra index values "seqNum" imported from mzML/mzXML files. 
#'
#' @return a matrix
#'

.grid_matrix <- function(mzDt, seqNums){
  nSeqNums <- length(seqNums)
  colIndexs <- c(1L:nSeqNums)
  nBins <- mzDt[, max(mzGrid_index)]
  
  #add column index to mzDt
  for (i in colIndexs) {
    mzDt[seqNum == seqNums[i], colIndex := i]
  }
  
  #Allocate
  m <- matrix(data = 0, ncol = nSeqNums, nrow = nBins)
  
  #Assign intensity to n column matrix
  m[cbind(mzDt$mzGrid_index, mzDt$colIndex)] <- mzDt$intensity
  m
}












#DEPRICATED: 
#Grid data to two vectors
#This is slower and less flexible than filling in matrix using .grid_matrix
.grid_vectors <- function(mzDt){
  seqNums <- unique(mzDt$seqNum)
  nBins <- mzDt[, max(mzGrid_index)]
  
  v1 <- v2 <- vector(mode = "numeric", length = nBins)
  v1[mzDt[seqNum == seqNums[1], mzGrid_index]] <- mzDt[seqNum == seqNums[1], intensity]
  v2[mzDt[seqNum == seqNums[2], mzGrid_index]] <- mzDt[seqNum == seqNums[2], intensity]
  
  list(v1, v2)
}

.computeSimilarity_pairMethod <- function(mzDt, ppmTol, isCentroid, intensityMin = 0, normalize = TRUE, method = "sqrt"){
  #Normalize intensity
  if(normalize){
    normIntensity <- .normalizeSpectrum_dt(mzDt, method)
    #Replace summed intensity with normalized summed intensity
    mzDt[, intensity := NULL]
    mzDt[, intensity := normIntensity]
  }
  
  #Index to master grid
  uniqueSpectra <- unique(mzDt$seqNum)
  nSpectra <- length(uniqueSpectra)
  specCombos <- combn(x = uniqueSpectra, m = 2, simplify = FALSE) #by list element
  
  #If you already have future running, can you pass this to future-lapply... mzDt could be huge?
  specPairGrids <- lapply(FUN = indexMasterSpectrum_pair, 
                          X = specCombos, 
                          ppmTol = ppmTol, 
                          isCentroid = isCentroid,
                          mzDt = mzDt[intensity > intensityMin, list(seqNum, mz, intensity)])
  
  #Process spectral pairs (by list element or rbindlist?)
  #specPairGrids <- data.table::rbindlist(specPairGrids, idcol = "seqNum_pairIndex")
  
  lapply(X = specPairGrids, FUN = .sim_dotProd)
}
