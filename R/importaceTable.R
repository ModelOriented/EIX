#'Importance table
#'
#'Function \code{importaceTable} returns table with importance of single variable and interactions with the following measure:
#'\itemize{
#'\item sumGain
#'\item sumCover
#'\item mean5Gain
#'\item meanGain
#'\item meanCover
#'\item freqency
#'}
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



importanceTable<-function(xgb.model, data,trees = NULL){

parentsGain<-childsGain<-name_pair<-Cover<-Feature<-Gain<-indx<-.<-NULL

  trees<-gainsInteractions(xgb.model, data,trees)
  trees<-trees[,interaction:=(parentsGain<childsGain)]


  importance<-trees[interaction==FALSE]
  importance<-importance[,.(Feature=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 1))),Gain=parentsGain,Cover)]
  importance2<-importance

  importance<-trees[interaction==FALSE]
  importance<-importance[,.(Feature=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 2))),Gain=childsGain, Cover)]
  importance2<-rbind(importance, importance2)

  #interakcje
  importance<-trees[interaction==TRUE]
  importance<-importance[,.(Feature=name_pair,Gain=childsGain, Cover)]
  importance2<-rbind(importance, importance2)


  setorder(setDT(importance2), Feature, -Gain)[, indx := seq_len(.N), by = Feature]
  importanceTop<-importance2[indx <= 5]
  importance4<-importanceTop[,.(mean5Gain=mean(Gain)), by=Feature]

  importance3<-importance2[,.(sumGain=sum(Gain), sumCover=sum(Cover),meanGain=mean(Gain), meanCover=mean(Cover), frequency=.N),by=Feature]

  importance5<-merge(importance3,importance4, by="Feature")
  setorderv(importance5, "sumGain",-1)
  return(importance5[])
}
