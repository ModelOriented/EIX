#'Gain plot for pairs
#'
#'Function \code{showPairsGainPlot} returns plot with gains of each pairs in the model.
#'
#'NOTE Two variable are pairs when they is located in the model one below the other. If sum of gains is high, it does not mean, that there is strong interaction between this two variable.
#'To see the same plot for strong interactions see \code{showInterantionsGainPlot}.
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


showPairsGainPlot<-function(xgb.model,data, trees = NULL){

  down<-up<-sumGain<-breaks<-NULL

  gainTable<-calculateGainTable(xgb.model,data, trees)
  gainTable$breaks <- cut(gainTable$sumGain,breaks = 4,right = FALSE, dig.lab = 5)

  ggplot(data.frame(gainTable), aes(down, up, sumGain)) +
    geom_tile(aes(fill = breaks)) +
    theme_mi2() +
    theme(axis.text.x = element_text(angle = 90))+
    scale_fill_manual(name = "sumGain", values = c("#ffffff","#ccccff","#7f7fff","#3232ff"), drop=FALSE,breaks = levels(gainTable$breaks), labels= levels(gainTable$breaks))
}
