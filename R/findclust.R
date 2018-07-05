#' Filters list to contain only desired clusters
#'
#' Filters list to contain only desired clusters
#'
#' @param data a list of data matrices with list elements corresponding to clusters and matrices of intensities of measured parameters
#' @param clusters indices of the clusters to retain
#'
#' @return A list of data matrices for the desired clusters
#'
#' @examples
#' library(mineCitrus)
#' data("citrus.combinedFCSSet")
#' data("citrus.foldClustering")
#' data("citrus.foldFeatureSet")
#' meds<-allmeds(citrus.combinedFCSSet=citrus.combinedFCSSet,
#'               citrus.foldClustering=citrus.foldClustering,
#'               citrus.foldFeatureSet=citrus.foldFeatureSet)
#' filteredmeds<-findclust(data=meds,clusters=c(19999,19972,19988))
#' @export
findclust<-function(data,clusters){
  clustind<-c()
  for(i in clusters){
    clustind<-c(clustind,grep(i,names(data)))
  }
  return(data[clustind])
}
