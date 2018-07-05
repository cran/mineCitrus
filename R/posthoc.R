#' Runs ANOVA and t-tests comparing clusters and markers in clusters
#'
#' Runs ANOVA and t-tests comparing clusters and markers in clusters
#'
#' @param processedDat data that has been processed using the processforanova function
#' @param clustIDdif ID number of the cluster to compare the others to
#'
#' @return A list of t-test results for each of the comparisons
#'
#' @importFrom stats aggregate aov median t.test
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
#' meds2<-filterMarker(clustdat=filteredmeds,markers=c(2,3))
#' foranova<-processforanova(filtereddata=meds2)
#' ttests<-posthoc(processedDat=foranova,clustIDdif=19999)
#' @export
posthoc<-function(processedDat,clustIDdif){
  anovares<-aov(markerDat~clusterID,data=processedDat)
  pval<-unlist(summary(anovares))
  pval<-pval["Pr(>F)1"]

  allmarkers<-unique(processedDat$markerID)
  clusterIDs<-unique(processedDat$clusterID)
  comp<-grep(clustIDdif,clusterIDs)
  ind<-1:length(clusterIDs)
  others<-ind[-comp]
  ttests<-list()
  index<-1
  for(i in allmarkers){

    data<-processedDat[processedDat$markerID==i,]
    t<-list()
    count<-1
    for(j in others){
      t[[count]]<-t.test(asinh(data[data[,2]==clusterIDs[comp],1]),asinh(data[data[,2]==clusterIDs[j],1]))
      count<-count+1
    }
    ttests[[index]]<-t
    index<-index+1
  }
  names(ttests)<-allmarkers
  return(ttests)
}
