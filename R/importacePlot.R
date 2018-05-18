#'Importance plot
#'
#'Plot two chosen measures of variables importance in the xgboost model.
#'
#'Avaible measures:
#'\itemize{
#'\item "sumGain"
#'\item "sumCover"
#'\item "mean5Gain"
#'\item "meanGain"
#'\item "meanCover"
#'\item "freqency"
#'}
#'
#'Aditionally for plots with single variable:
#'\itemize{
#'\item "meanDepth",
#'\item "counterRoot",
#'\item "weightedRoot",
#'}
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param xlab measure on the x-axis. Default "sumCover"
#' @param ylab measure on the y-axis. Default "sumGain"
#' @param opt  "single", "mixed","interactions". Default "mixed"
#' @param top number of variables on the plot or NULL for all variable. Default NULL
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#' @import ggplot2
#' @import data.table
#' @import DALEX
#'
#'
#' @export


importancePlot<-function(xgb.model,data,xlab="sumCover", ylab="sumGain", opt="mixed", top=10,trees = NULL){

  if(opt=="single"){
    importance<-importanceSingleVariable(xgb.model,data,trees)
  }
 if(opt=="mixed"){
   importance<-importanceTable(xgb.model,data,trees)
 }
 if(opt=="interactions"){
    importance<-importanceInteractions(xgb.model,data,trees)
  }

  if( top=="NULL"){
    ggplot(data.frame(importance), aes_string(x=xlab, y=ylab,label="Feature"))+
      geom_point()+
      scale_size()+geom_label(hjust=0, vjust=0)+theme_mi2()
  }else{
    ggplot(data.frame(importance[1:top,]), aes_string(x=xlab, y=ylab,label="Feature"))+
      geom_point()+
      scale_size()+geom_label(hjust=0, vjust=0)+theme_mi2()
  }
}
