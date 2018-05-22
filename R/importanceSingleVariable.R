#'Impotrance table for single variable
#'
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
#' @export

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
