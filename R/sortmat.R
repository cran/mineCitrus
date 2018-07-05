#' Reorders to rows (corresponding to different clusters) of a matrix of medians to a desired order
#'
#' Reorders to rows (corresponding to different clusters) of a matrix of medians to a desired order
#'
#' @param mat matrix of median data
#' @param desiredorder row labels from matrix in desired order
#'
#' @return Returns a matrix with rows rearranged in desired order
#'
#' @examples
#' library(mineCitrus)
#' data("citrus.combinedFCSSet")
#' data("citrus.foldClustering")
#' data("citrus.foldFeatureSet")
#' medians<-clustermeds(citrus.foldFeatureSet=citrus.foldFeatureSet,
#'                      citrus.foldClustering=citrus.foldClustering,
#'                      medsofinterest=c("Red","Blue"),
#'                      citrus.combinedFCSSet=citrus.combinedFCSSet)
#' names<-rownames(medians)
#' names<-names[c(31,1:30)]
#' sortedmedians<-sortmat(mat=medians,desiredorder=names)
#' @export
sortmat<-function(mat,desiredorder){
  ind<-c()
  for(i in desiredorder){
    ind<-c(ind,grep(i,rownames(mat)))
  }
  return(mat[ind,])
}
