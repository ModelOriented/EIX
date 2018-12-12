#' Interactions Plot
#'
#' Interactions Plot
#'
#' NOTE: High gain of pair for \code{opt="pairs"} can be a result of high gain of down variable (child).
#'      As strong interactions should be considered only these pairs of variables,
#'      where variable on the bottom (child) has higher gain than variable on the top (parent).
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#' @param opt if "interactions" then strong interactions will be presented on the plot,
#'            if "pairs" then plot presents all pairs in the model. Default "interactions".
#'
#'
#' @return a ggplot object
#'
#' @import data.table
#' @import ggplot2
#' @import DALEX
#' @import purrr
#'
#' @examples
#'
#' @export


interactionsPlot<-function(xgb.model,data, opt="interactions"){

  Feature<-sumGain<-Child<-Parent<-breaks<-NULL

  if(opt=="interactions"){
  gainTable<-importanceInteraction(xgb.model,data)[,Feature,sumGain]
  gainTable<-gainTable[,`:=`(Parent=as.vector(unlist(map(strsplit(gainTable[,Feature], "[:]"), 1))),Child=as.vector(unlist(map(strsplit(gainTable[,Feature], "[:]"), 2))))]
  gainTable<-gainTable[,-2]
  }
  if(opt=="pairs"){
  gainTable<-calculatePairsGainTable(xgb.model,data)
  }

  sumGain<-gainTable$sumGain
  breaks<-c(min(sumGain),((max(sumGain-min(sumGain)))/4),((max(sumGain-min(sumGain)))/2),(3*((max(sumGain-min(sumGain)))/4)),max(sumGain))
  gainTable$breaks <- cut(sumGain,breaks =breaks ,right = FALSE, dig.lab = 4, include.lowest=TRUE)
  gainTable$Child <- factor(gainTable$Child, levels = unique(gainTable$Child[order(gainTable$sumGain,decreasing = TRUE)]))

  ggplot(data.frame(gainTable), aes(Child, Parent, sumGain)) +
    geom_tile(aes(fill = breaks)) +
    theme_mi2() +
    theme(axis.text.x = element_text(hjust=1, angle=90),
          axis.text.y=element_text(hjust=1, angle=0))+
    scale_fill_manual(name = "sumGain", values = c("#ffffff","#ccccff","#7f7fff","#3232ff"), drop=FALSE,breaks = levels(gainTable$breaks), labels= c("very low","low", "medium","high"))+
    coord_equal()
}

