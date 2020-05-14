#' parallalized mzML import - does not work :(
#'
#' @param path a system path to the .mzML file
#' @param scans A numeric specifying which scans to return. Optional argument.
#'   If ommited, the requested data for all peaks is returned.
#'
#' @return
#' @export
#'
#' @examples
mzML2dataTable_par <- function(path, scans = NULL, nthreads = NULL){
  #Link to the file
  file <- mzR::openMSfile(filename = path, verbose = TRUE)

  #Import header data
  if(is.null(scans)){
    header <- mzR::header(object = file)
    scans <- c(1:nrow(header))
  }else{
    header <- mzR::header(object = file, scans = scans)
  }

  #Generates list of vectors for group indexing
  indexList <- splitIndex(nCores = parallel::detectCores(), index = scans)
  scanNumbers <-indexList$groups      #actual scan numbers randomized in their groups
  scanIndex <- indexList$groupIndex   #index of the scan number

  #Split the header by the scanIndex
  header <- mapply(FUN = splitDFbyVectorList,
                   vectorList = scanIndex,
                   MoreArgs = list(header),
                   SIMPLIFY = FALSE)

  l <- BiocParallel::bpmapply(FUN = mzML2dataTable,
                              scans = scanNumbers,
                              header = header,
                              MoreArgs = list(path = path),
                              SIMPLIFY = FALSE)

  data.table::rbindlist(l)
}

splitDFbyVectorList <- function(df, vectorList){
  df[vectorList, ]
}

