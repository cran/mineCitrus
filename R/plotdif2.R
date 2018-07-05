#' Plot dot plots of features where one cluster is significantly different from the reference cluster
#'
#' Plot dot plots of features where one cluster is significantly different from the reference cluster
#'
#' @param BJHdf results of a call to findsig
#' @param anovadata results of call to processforanova
#' @param strat clusterIDs for clusters that are stratifying
#'
#' @return Dot plots for all features where one cluster is significantly different from the reference cluster

#' @import ggplot2
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
#' sig<-findSig(posHocRes=ttests)
#' graphs<-plotdif2(BJHdf=sig,anovadata=foranova,strat=19999)
#' @export
plotdif2<-function(BJHdf,anovadata,strat){
  a<-strsplit(rownames(BJHdf[BJHdf$BJHsig==T,]),".p.value")
  b<-c()
  for(i in a){
    b<-c(b,i[1])
  }
  c<-unique(b)
  b<-unique(b[duplicated(b)])
  c<-setdiff(c,b)
  c<-gsub("\\.","-",c)
  anovadata["strat"]<-"F"
  for(i in strat){
    anovadata[anovadata$clusterID==i,4]="T"
  }
  plots<-ggplot(anovadata[anovadata$markerID==c,],aes(x=anovadata$clusterID,y=asinh(anovadata$markerDat),color=anovadata$strat))+stat_boxplot(geom='errorbar',color="black")+geom_boxplot(color="black")+geom_point()+scale_color_manual(values=c("T"='#ff0000',"F"='#0000FF'))+labs(x="",y="Arcsinh MFI")+theme(legend.position = "none",strip.background = element_blank())+facet_wrap(~anovadata$markerID)+annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
  return(plots)
}
