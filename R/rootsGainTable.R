#'
#'Function \code{rootsGainTable}
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'@import data.table
#'@import xgboost
#'
#'
#'


rootsGainTable<-function(xgb.model, data,trees = NULL){

  Node<-Quality<- Feature<-Tree<-ID<-depth<-.<-NULL

  trees = rbindlist(calculateDepth(xgb.model, data,trees))
  roots<-trees[Node==0, .(Quality, Feature, Tree, ID,depth)]

  return(roots[])

}
