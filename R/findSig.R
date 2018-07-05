#' Assesses significance of ANOVA and t-test results
#'
#' Assesses significance of ANOVA and t-test results
#'
#' @param posHocRes results from a call to the posthoc function
#'
#' @return A dataframe indicating the significances of results

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
#' sig<-findSig(posHocRes=ttests)
#' @export
findSig<-function(posHocRes){
  everything<-unlist(posHocRes)
  valuenames<-names(everything)
  pvaluesind<-grep("p.value",valuenames)
  keepnames<-valuenames[pvaluesind]
  pvals<-as.numeric(everything[pvaluesind])
  names(pvals)<-keepnames
  rankings<-rank(pvals,ties.method = "random")
  cutoffs<-(1:length(pvals))*0.05/length(pvals)
  cutoffs<-cutoffs[rankings]
  pvaldf<-data.frame(pvals=pvals,rankings=rankings,cutoffs=cutoffs)
  pvaldf["belowCutoff"]<-pvaldf$pvals<pvaldf$cutoffs
  pvaldf[is.na(pvaldf$pval),4]=F
  maxsigrank<-max(pvaldf[pvaldf$belowCutoff==T,2])
  pvaldf["BJHsig"]<-F
  pvaldf[pvaldf$rankings<=maxsigrank,5]<-T
  rownames(pvaldf)<-make.names(keepnames,unique=T)
  return(pvaldf)
}
