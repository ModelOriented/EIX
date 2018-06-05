#'Gain for interactions
#'
#'Function \code{showInteractionsPlot} returns plot with gains of each strong interaction in the model.
#'
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#' @param opt "interactions", "pairs"
#'
#'@import data.table
#'@import ggplot2
#'@import DALEX
#'@import purrr
#'
#'
#' @export


showInteractionsPlot<-function(xgb.model,data, opt="interactions",trees = NULL){

  Feature<-sumGain<-child<-parent<-breaks<-NULL

  if(opt=="interactions"){
  gainTable<-importanceInteraction(xgb.model,data, trees)[,Feature,sumGain]
  gainTable<-gainTable[,`:=`(parent=as.vector(unlist(map(strsplit(gainTable[,Feature], "[:]"), 1))),child=as.vector(unlist(map(strsplit(gainTable[,Feature], "[:]"), 2))))]
  gainTable<-gainTable[,-2]
  }
  if(opt=="pairs"){
  gainTable<-calculatePairsGainTable(xgb.model,data, trees)
  }

  sumGain<-gainTable$sumGain
  breaks<-c(min(sumGain),((max(sumGain-min(sumGain)))/4),((max(sumGain-min(sumGain)))/2),(3*((max(sumGain-min(sumGain)))/4)),max(sumGain))
  gainTable$breaks <- cut(sumGain,breaks =breaks ,right = FALSE, dig.lab = 4, include.lowest=TRUE)
  gainTable$child <- factor(gainTable$child, levels = unique(gainTable$child[order(gainTable$sumGain,decreasing = TRUE)]))

  ggplot(data.frame(gainTable), aes(child, parent, sumGain)) +
    geom_tile(aes(fill = breaks)) +
    theme_mi2() +
    theme(axis.text.x = element_text(angle = 90))+
    scale_fill_manual(name = "sumGain", values = c("#ffffff","#ccccff","#7f7fff","#3232ff"), drop=FALSE,breaks = levels(gainTable$breaks), labels= levels(gainTable$breaks))+
    coord_equal()
}

