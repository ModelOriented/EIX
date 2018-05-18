#'Weighted mean depth
#'
#'This function calculates weighted depth mean for every variable.
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'@import data.table
#'
#'
#'
#' @export

calculateWeightedDepth <-function(xgb.model,data, trees = NULL){

  Feature<-depth<-Quality<-.<-NULL

  treeList = calculateDepth(xgb.model, data, trees)
  trees<-rbindlist(treeList)
  trees<-trees[Feature!="Leaf"]
  trees<-trees[, .(meanDepth=weighted.mean(depth, Quality),count=.N),by=Feature]

  return(trees[])
}


