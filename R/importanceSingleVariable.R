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

#do poprawy
trees2 = xgb.model.dt.tree(colnames(data), model = xgb.model, trees )
trees2<-trees2[Feature!="Leaf",Gain:=Quality]
trees2<-trees2[,.(Feature,Gain,Cover)]

importance5<-countRoots(xgb.model, data,trees)
importance6<-calculateWeightedDepth(xgb.model, data,trees)


setorder(setDT(trees2), Feature, -Gain)[, indx := seq_len(.N), by = Feature]
importanceTop<-trees2[indx <= 5]
importance4<-importanceTop[,.(mean5Gain=mean(Gain)), by=Feature]
importance3<-trees2[Feature!="Leaf",.(sumGain=sum(Gain), sumCover=sum(Cover),meanGain=mean(Gain), meanCover=mean(Cover), frequency=.N),by=Feature]

importance7<-merge(importance5,importance6, by="Feature", all=TRUE)[,-"sumGain"]
importance8<-merge(importance3,importance4, by="Feature")
importance9<-merge(importance8,importance7, by="Feature")[,-"count"]

setorderv(importance9, "sumGain",-1)
importance9[is.na(importance9)]<-0
return(importance9[])

}
