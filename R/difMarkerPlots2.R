#' Plot dot plots of features where one cluster is significantly different from the reference cluster without processing data before hand
#'
#' Plot dot plots of features where one cluster is significantly different from the reference cluster without processing data before hand
#'
#' @param data output from call to allmeds function
#' @param clusters clusterIDs of the desired clusters to compare and plot
#' @param markers indices of the columns of the data matrix for features to be analyse
#' @param diffclust clusterID of for cluster to statisticaly compare others to
#' @param strat clusterIDs for stratifying clusters as indicated by Citrus results
#'
#' @return Dot plots for all features where one cluster is significantly different from the reference cluster

#' @import ggplot2
#'
#' @examples
#' library(mineCitrus)
#' data("citrus.combinedFCSSet")
#' data("citrus.foldClustering")
#' data("citrus.foldFeatureSet")
#' meds<-allmeds(citrus.combinedFCSSet=citrus.combinedFCSSet,
#'               citrus.foldClustering=citrus.foldClustering,
#'               citrus.foldFeatureSet=citrus.foldFeatureSet)
#' graphs<-difMarkerPlots2(data=meds,clusters=c(19999,19972,19988),markers=c(2,3),
#'                         diffclust=19999,strat=19999)
#' @export
difMarkerPlots2<-function(data,clusters,markers,diffclust,strat){
  clustofinterest<-findclust(data=data,clusters = clusters)
  clusterWithFilteredMarkers<-filterMarker(clustdat=clustofinterest,markers=markers)
  datforanova<-processforanova(filtereddata=clusterWithFilteredMarkers)
  stats<-posthoc(processedDat=datforanova,clustIDdif=diffclust)
  sigstats<-findSig(stats)
  plots<-plotdif2(BJHdf=sigstats,anovadata = datforanova,strat=strat)
  return(plots)
}
