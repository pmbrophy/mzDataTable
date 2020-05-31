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
#'  simMat <- computeSimilarity(mzDt = dt[msLevel == 2 & seqNum %between% c(1, 10)], 
#'                              ppmTol = 100, 
#'                              isCentroid = TRUE, 
#'                              intensityMin = 0.01, 
#'                              normalize = TRUE, 
#'                              normalization_method = "sqrt", 
#'                              similarity_method = NULL)
#' 

computeSimilarity <- function(mzDt, ppmTol, isCentroid, intensityMin = 0, normalize = TRUE, normalization_method = "sqrt", similarity_method){
  #Normalize intensity
  if(normalize){
    normIntensity <- .normalizeSpectrum_dt(mzDt, normalization_method)
    #Replace summed intensity with normalized summed intensity
    mzDt[, intensity := NULL]
    mzDt[, intensity := normIntensity]
  }
  
  #Grid all spectra onto a single grid - filter intensity, only take necessary columns
  mzDt_grid <- .normalizeEachSpectrum_dt(mzDt = mzDt[intensity >= intensityMin, list(seqNum, mz, intensity)], 
                                         ppmTol = ppmTol, 
                                         isCentroid = isCentroid) 
  
  similarity <- .sim_dotProd(mzDt = mzDt_grid, similarity_method = similarity_method)
  
  similarity
}


#TODO: Need to modularize to allow for multiple methods
#Dot product method 
.sim_dotProd <- function(mzDt, similarity_method){
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
    
    ##Dot Product Similarity 
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
    
    ##Spectral Contrast Angle
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- m[,col1] %*% m[,col2]
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }else if(similarity_method == "specCor"){
    
    ##Spectral Contrast Angle
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- m[,col1] %*% m[,col2]
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }else if(similarity_method == "brayCurtis"){
    
    ##Spectral Contrast Angle
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- m[,col1] %*% m[,col2]
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }else if(similarity_method == "euclidean"){
    
    ##Spectral Contrast Angle
    for (i in comboIndexs) {
      result_loc[,1] <- i
      
      col1 <- colCombos[1,i]
      col2 <- colCombos[2,i]
      
      result[1] <- m[,col1] %*% m[,col2]
      result[2] <- seqNums[col1]
      result[3] <- seqNums[col2]
      
      sims[result_loc] <- result
    }
    
  }
  
  #Return Matrix
  sims
}

#Grid data to a maxrix with two or more columns (This is faster than doing it with 2 vectors)
#Each seqNum gets its own column
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




computeSimilarity_pairMethod <- function(mzDt, ppmTol, isCentroid, intensityMin = 0, normalize = TRUE, method = "sqrt"){
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
