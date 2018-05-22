#'Importance table for interactions.
#'
#'Function \code{importanceInteractions} returns table with importance of interactions with the following measure:
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
#' @export


importanceInteractions<-function(xgb.model,data, trees = NULL){

  parentsGain<-childsGain<-name_pair<-Cover<-.<-Feature<-Gain<-indx<-NULL

  #importance<-importanceTable(xgb.model,data, trees)
  trees<-gainsInteractions(xgb.model, data,trees)
  trees<-trees[interaction==TRUE]
  tress<-trees[,`:=`(Feature=name_pair,Gain=childsGain)]
  tress<-trees[,.(Feature,Gain, Cover)]
  importance<-merge(trees[,.(sumGain=sum(Gain), sumCover=sum(Cover),meanGain=mean(Gain), meanCover=mean(Cover), frequency=.N),by=Feature],mean5gain(trees), by="Feature")
  setorderv(importance, "sumGain",-1)

  return(importance[])
}
