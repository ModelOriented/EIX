#'Gain plot for interactions
#'
#'Function \code{showInteractionsGainPlot} returns plot with gains of each strong interaction in the model.
#'
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#'@import data.table
#'@import ggplot2
#'@import DALEX
#'
#'
#' @export


showInteractionsGainPlot<-function(xgb.model,data, trees = NULL){

  Feature<-sumGain<-down<-up<-breaks<-NULL

  gainTable<-importanceInteractions(xgb.model,data, trees )[,Feature,sumGain]
  gainTable<-gainTable[,`:=`(up=as.vector(unlist(map(strsplit(gainTable[,Feature], "[:]"), 1))),down=as.vector(unlist(map(strsplit(gainTable[,Feature], "[:]"), 2))))]
  gainTable<-gainTable[,-2]
  gainTable$breaks <- cut(gainTable$sumGain,breaks = 4,right = FALSE, dig.lab = 5)

  ggplot(data.frame(gainTable), aes(down, up, sumGain)) +
    geom_tile(aes(fill = breaks)) +
    theme_mi2() +
    theme(axis.text.x = element_text(angle = 90))+
    scale_fill_manual(name = "sumGain", values = c("#ffffff","#ccccff","#7f7fff","#3232ff"), drop=FALSE,breaks = levels(gainTable$breaks), labels= levels(gainTable$breaks))
}
