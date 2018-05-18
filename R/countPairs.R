#'Table with namber of pairs
#'
#'Function \code{countPairs} counts how many times each pair of variable occures in the model.
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'@import data.table
#'@import stats
#'@import utils
#'@import tidyr
#'@import purrr
#'
#'
#' @export



countPairs<- function(xgb.model,data, trees = NULL) {
  V1<-down<-N<-NULL

  treeList<-calculateGain(xgb.model,data, trees)
  trees<-rbindlist(treeList)
  importance<-data.table(table(trees[,"name_pair"]))
  importance<-na.omit(importance)
  importance<-importance[,`:=`(up=as.vector(unlist(map(strsplit(as.character(importance[,V1]), "[:]"), 1))), down=as.vector(unlist(map(strsplit(importance[,V1], "[:]"), 2))))]
  importance<-importance[,-1]
  importance<-spread(importance,down,N)
  importance[is.na(importance)]<-'.'
  return(importance[])
}



