#'gainsInteractions
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



gainsInteractions<-function(xgb.model,data, trees = NULL){

  Tree<-name_pair<-parentsGain<-childsGain<-.<-Cover<-parentsCover<-NULL

  treeList<-calculateGain(xgb.model,data, trees)
  trees<-rbindlist(treeList)
  trees<-na.omit(trees[,.(Tree,name_pair, parentsGain, childsGain, Cover, parentsCover)])

  return(trees[])
}
