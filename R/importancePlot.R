#' Importance plot
#'
#' The plot containing two chosen measures of variables' importance in the model.
#'
#' Available measures:
#'\itemize{
#'\item "sumGain" - sum of Gain value in all nodes, in which given variable occurs
#'\item "sumCover" - sum of Cover value in all nodes, in which given variable occurs; for LightGBM models: number of observation, which pass through the node
#'\item "mean5Gain" - mean gain from 5 occurrences of given variable with the highest gain
#'\item "meanGain" - mean Gain value in all nodes, in which given variable occurs
#'\item "meanCover" - mean Cover value in all nodes, in which given variable occurs; for LightGBM models: mean number of observation, which pass through the node
#'\item "freqency" - number of occurrences in the nodes for given variable
#'}
#'
#' Additionally for plots with single variables:
#'\itemize{
#'\item "meanDepth"  - mean depth weighted by gain
#'\item "counterRoot" - number of occurrences in the root
#'\item "weightedRoot" - mean number of occurrences in the root, which is weighted by gain
#'}
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#' @param xlab measure on the x-axis. Default "sumCover"
#' @param ylab measure on the y-axis. Default "sumGain"
#' @param opt if "single" then plot includes only single variable,
#'            if "interactions", then only interactions
#'            if "mixed", then both single variable and interactons.
#'            Default "mixed".
#' @param top number of positions on the plot or NULL for all variable. Default 10.
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @import data.table
#' @import DALEX
#' @import ggrepel
#'
#' @examples
#'
#' @export


importancePlot<-function(xgb.model,data,xlab="sumCover", ylab="sumGain", opt="mixed", top=10){

importance<-importanceTable(xgb.model,data,opt)

  if( top=="NULL"){
    ggplot(data.frame(importance), aes_string(x=xlab, y=ylab,label="Feature"))+
      geom_point()+
      scale_size()+geom_label_repel()+theme_mi2()
  }else{
    ggplot(data.frame(importance[1:top,]), aes_string(x=xlab, y=ylab,label="Feature"))+
      geom_point()+
      scale_size()+geom_label_repel()+theme_mi2()
  }
}
