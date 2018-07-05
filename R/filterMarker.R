#' Filters list of data matrices with columns corresponding to the measured parameters of interest
#'
#' Filters list of data matrices with columns corresponding to the measured parameters of interest
#'
#' @param clustdat a list of data matrices with list elements corresponding to clusters and matrices of intensities of measured parameters
#' @param markers Indices of the columns of parmeters to keep
#'
#' @return A list of data matrices with columns of data matrices only corresponding to measured parameters of interest
#'
#' @examples
#' library(mineCitrus)
#' data("citrus.combinedFCSSet")
#' data("citrus.foldClustering")
#' data("citrus.foldFeatureSet")
#' meds<-allmeds(citrus.combinedFCSSet=citrus.combinedFCSSet,
#'               citrus.foldClustering=citrus.foldClustering,
#'               citrus.foldFeatureSet=citrus.foldFeatureSet)
#' meds2<-filterMarker(clustdat=meds,markers=c(2,3))
#' @export
filterMarker<-function(clustdat,markers){
  for(i in 1:length(clustdat)){
    clustdat[[i]]<-clustdat[[i]][,markers]
  }
  if(length(markers)>1){
    colnames(clustdat[[i]])<-names(markers)
  }else{
    names(clustdat[[i]])<-names(markers)
  }
  return(clustdat)
}
