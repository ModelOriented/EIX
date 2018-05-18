#'Table of pairs' gains
#'
#'
#' Function \code{calculateGainTable} returns table of gains of all variables' pair occures in the model.
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'@import stats
#'@import utils
#'@import data.table
#'@import purrr
#'@import tidyr
#'
#'
#'
#' @export





calculateGainTable<-function(xgb.model,data, trees = NULL){

  name_pair<-childsGain<-.<-NULL

  treeList<-calculateGain(xgb.model,data, trees)
  trees<-rbindlist(treeList)

  importance<- trees[,.(sumGain=sum(childsGain)),by=name_pair]
  importance<-na.omit(importance)
  importance<-importance[,`:=`(up=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 1))),down=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 2))))]
  importance<-importance[,-1]
  #importance<-spread(importance,down,sumGain)
  #importance[is.na(importance)]<-'.'

  return(importance[])

}
