#' Processes cluster signaling data in form for statistical analysis
#'
#' Processes cluster signaling data in form for statistical analysis
#'
#' @param filtereddata a list with each element corresonding to a cluster of interest and matrices containing individual sample data for desired markers
#' @return A dataframe sufficient for using the posthoc function to compute statistics
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
#' foranova<-processforanova(filtereddata=filteredmeds)
#' @export
processforanova<-function(filtereddata){
  clustids<-names(filtereddata)
  dimmat<-nrow(filtereddata[[1]])*ncol(filtereddata[[1]])
  markers<-colnames(filtereddata[[1]])
  clustlab<-c()
  vectdat<-c()
  markerlab<-c()
  for(i in 1:length(clustids)){
    clustlab<-c(clustlab,rep(clustids[[i]],dimmat))
    markerlab<-c(markerlab,rep(markers,each=nrow(filtereddata[[i]])))
    vectdat<-c(vectdat,as.vector(as.matrix(filtereddata[[i]])))
  }
  clustlab<-as.factor(clustlab)
  markerlab<-as.factor(markerlab)

  df<-data.frame(markerDat=vectdat,clusterID=clustlab,markerID=markerlab)
  return(df)
}
