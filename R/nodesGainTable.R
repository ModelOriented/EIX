#'
#'Function \code{nodesGainTable}
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
#'


nodesGainTable<-function(xgb.model, data,trees = NULL){

  Feature<-Quality<-Node<-Tree<-ID<-depth<-.<-NULL

  trees = rbindlist(calculateDepth(xgb.model, data,trees))
  nodes<-trees[Feature!="Leaf", .(Quality, Feature,Node,Tree, ID,depth)]

  return(nodes[])
}
