#'Importance Table Mixed
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'
#' @import data.table
#'
#'
#' @export





importanceTableMixed<-function(xgb.model, data, trees = NULL){

parentsGain<-childsGain<-name_pair<-Cover<-Feature<-Gain<-indx<-.<-NULL

trees<-gainsInteractions(xgb.model, data,trees)

#single variables
importance<-trees[interaction==FALSE]
importance1<-importance[,.(Feature=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 1))),Gain=parentsGain,Cover)]
importance2<-importance[,.(Feature=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 2))),Gain=childsGain, Cover)]
importance3<-rbind(importance1, importance2)

#interactions
importance<-trees[interaction==TRUE]
importance1<-importance[,.(Feature=name_pair,Gain=childsGain, Cover)]
importance3<-rbind(importance3, importance1)

importance4<-merge(importance3[,.(sumGain=sum(Gain), sumCover=sum(Cover),meanGain=mean(Gain), meanCover=mean(Cover), frequency=.N),by=Feature],mean5gain(importance3), by="Feature")
setorderv(importance4, "sumGain",-1)

return(importance4[])
}
