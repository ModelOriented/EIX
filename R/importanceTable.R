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
#' @param opt "single", "interactions","mixed". Default "mixed"
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#' @import data.table
#'
#'
#'
#' @export



importanceTable<-function(xgb.model, data,opt="mixed", trees = NULL){

  importance<-NULL

  if(opt=="mixed") {importance<- importanceTableMixed(xgb.model, data, trees)}
  if(opt=="single") {importance<-importanceSingleVariable(xgb.model, data, trees)}
  if(opt=="interactions") {importance<-importanceInteraction(xgb.model, data, trees)}

  importance<-cbind(importance[,1],signif(importance[,-1], digits = 4))

  return(importance[])

}

importanceTableMixed<-function(xgb.model, data, trees = NULL){

  parentsGain<-childsGain<-name_pair<-Cover<-Feature<-Gain<-indx<-.<-NULL

  trees<-gainInteractions(xgb.model, data,trees)

  #single variables
  importance<-trees[interaction==FALSE]
  importance1<-importance[,.(Feature=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 1))),Gain=parentsGain,Cover, interaction)]
  importance2<-importance[,.(Feature=as.vector(unlist(map(strsplit(importance[,name_pair], "[:]"), 2))),Gain=childsGain, Cover, interaction)]
  importance3<-rbind(importance1, importance2)


  #interactions
  importance<-trees[interaction==TRUE]
  importance1<-importance[,.(Feature=name_pair,Gain=childsGain, Cover, interaction)]
  importance3<-rbind(importance3, importance1)

  importance4<-merge(importance3[,.(sumGain=sum(Gain), sumCover=sum(Cover),meanGain=mean(Gain), meanCover=mean(Cover), frequency=.N),by=Feature],mean5gain(importance3), by="Feature")
  setorderv(importance4, "sumGain",-1)

  return(importance4[])
}


importanceInteraction<-function(xgb.model,data, trees = NULL){

  parentsGain<-childsGain<-name_pair<-Cover<-.<-Feature<-Gain<-indx<-NULL

  #importance<-importanceTable(xgb.model,data, trees)
  trees<-gainInteractions(xgb.model, data,trees)
  trees<-trees[interaction==TRUE]
  tress<-trees[,`:=`(Feature=name_pair,Gain=childsGain)]
  tress<-trees[,.(Feature,Gain, Cover)]
  importance<-merge(trees[,.(sumGain=sum(Gain), sumCover=sum(Cover),meanGain=mean(Gain), meanCover=mean(Cover), frequency=.N),by=Feature],mean5gain(trees), by="Feature")
  setorderv(importance, "sumGain",-1)

  return(importance[])
}


importanceSingleVariable<-function(xgb.model, data,trees = NULL){

  Feature<-Gain<-Quality<-Cover<-indx<-.<-NULL

  trees2 = xgb.model.dt.tree(colnames(data), model = xgb.model, trees )
  trees2<-trees2[Feature!="Leaf",Gain:=Quality]
  trees2<-trees2[,.(Feature,Gain,Cover)]

  importance1<-merge(countRoots(xgb.model, data,trees),calculateWeightedDepth(xgb.model, data,trees), by="Feature", all=TRUE)[,-"sumGain"]
  importance2<-merge(trees2[Feature!="Leaf",.(sumGain=sum(Gain), sumCover=sum(Cover),meanGain=mean(Gain), meanCover=mean(Cover), frequency=.N),,by=Feature],mean5gain(trees2), by="Feature")

  importance<-merge(importance1,importance2, by="Feature")[,-"count"]
  setorderv(importance, "sumGain",-1)
  importance[is.na(importance)]<-0

  return(importance[])

}


