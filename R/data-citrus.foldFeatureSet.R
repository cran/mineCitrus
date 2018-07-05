#' Correlation data for example of Citrus data set from nolanlab/citrus
#'
#' A dataset containing the association of red and blue in clusters with different sample groups
#'
#' @format A list with 8 elements:
#' \describe{
#'   \item{allFeatures}{Data set for each sample for all markers and clusters}
#'   \item{allLargeEnoughClusters}{Vector of clusters meeting size threshold}
#'   \item{foldFeatures}{Data for each fold clustering}
#'   \item{foldLargeEnoughClusters}{Clusters meeting size threshold for each fold clustering}
#'   \item{folds}{Descriptions of each data clustering}
#'   \item{leftoutFeatures}{Data omitted from analyses}
#'   \item{minimumClusterSizePercent}{Minimum size threshold to retain clusters in analysis}
#'   \item{nFolds}{The number of times data is clustered}
#'   ...
#' }
#' @source \url{https://github.com/nolanlab/citrus}
"citrus.foldFeatureSet"
