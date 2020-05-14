#' Split a vector into groups
#'
#' @param index the vector of index values to be split into groups
#' @param nGroups number of groups to be generated
#' @param randomize should the groups contain a random or ordered sampling from
#'   the index vector
#'
#' @return returns a list containing `groups` and `groupIndex` both of which are
#'   lists of length nGroups
#' @export
#'
#' @examples
#' index <- c(c(1:10), c(30:40))
#' groupedIndex <- splitIndex(nGroups = 3, index = index, randomize = TRUE)
#'
#' groups <- unlist(groupedIndex$groups)
#' groups_i <- unlist(groupedIndex$groupIndex)
#'
#' all(index %in% groups)
#'
#' #Reorder the radomized groups by groups_i
#' groups_reorder <- vector(mode = "numeric", length = length(groups))
#' groups_reorder[groups_i] <- groups
#' groups_reorder
#'
splitIndex <- function(index, nGroups, randomize = FALSE){
  indexLength <- length(index)

  #randomized index_i for the vector index
  index_i <- c(1:indexLength)
  index_i <- sample(index_i)

  #Calcualte the groups
  groupSize <- indexLength %/% nGroups
  groupStarts <- seq(from = 1, to = groupSize * nGroups, by = groupSize)
  groupStops <- groupStarts + (groupSize-1)
  groupStops[nGroups] <- groupStops[nGroups] + (length(index) %% nGroups)

  groups <- mapply(FUN = seq, from = groupStarts, to = groupStops, by = 1, SIMPLIFY = FALSE)

  #randomized group indexs
  groups_i <- mapply(FUN = indexFromVector, indexLocs = groups, MoreArgs = list(index_i), SIMPLIFY = FALSE)

  if(randomize){
    #Using randomized group indexs, get the actual index values
    groups <- mapply(FUN = indexFromVector, indexLocs = groups_i, MoreArgs = list(index), SIMPLIFY = FALSE)
  }else{
    #Ordered implementation
    groups <- mapply(FUN = indexFromVector, indexLocs = groups, MoreArgs = list(index), SIMPLIFY = FALSE)
  }

  list(groups = groups, groupIndex = groups_i)
}

#extract index locations from a vector
indexFromVector <- function(indexLocs, vec){
  vec[indexLocs]
}
