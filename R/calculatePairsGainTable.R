#' Table of pairs' gains
#'
#' Table containing gains of all variables' pairs occur in the model.
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#'
#' @return a data table
#'
#' @import stats
#' @import utils
#' @import data.table
#' @import purrr
#' @import tidyr
#'
#' @examples
#'
#' @export

calculatePairsGainTable<-function(xgb.model,data){

  name_pair<-childsGain<-.<-NULL

  treeList<-calculateGain(xgb.model,data)
  trees<-rbindlist(treeList)

  importance<- trees[,.(sumGain=sum(childsGain)),by=name_pair]
  importance<-na.omit(importance)
  importance<-importance[,`:=`(Parent=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 1))),Child=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 2))))]
  importance<-importance[,-1]
  setorderv(importance, "sumGain",-1)

  return(importance[])

}
