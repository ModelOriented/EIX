#'ggpairsPlot
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#'
#' @return a ggplot object
#'
#' @import data.table
#' @import ggplot2
#' @import GGally
#'
#' @examples
#'
#' @export

ggpairsPlot<-function(xgb.model, data){
ggpairs(importanceTable(xgb.model,data,opt="single")[,2:ncol(importanceTable(xgb.model,data,opt="single"))])#+theme_mi2()
}
