#' Clustering data for example of Citrus data set from nolanlab/citrus
#'
#' A dataset containing the clustering of different cell groups
#'
#' @format A large citrus.foldClustering object with 5 elements:
#' \describe{
#'   \item{allClustering}{A list describing which events belong to which clusters}
#'   \item{foldClustering}{A list describing which events belong to which clusters for each fold}
#'   \item{foldMappingAssignments}{A list describing assignments with fold clustering}
#'   \item{folds}{Descriptions of each data clustering}
#'   \item{nFolds}{The number of times data is clustered}
#'   ...
#' }
#' @source \url{https://github.com/nolanlab/citrus}
"citrus.foldClustering"
