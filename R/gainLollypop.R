#'Visualiation of xgboost model
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




gainLollypop<- function(xgb.model, data,trees = NULL){

  Tree<-Quality<-depth<-Feature<-NULL

  nodes<-nodesGainTable(xgb.model, data,trees)
  roots<-rootsGainTable(xgb.model, data,trees)

  p<-ggplot(data=data.frame(nodes), aes(x=Tree, y=Quality,group=as.factor(depth), label=Feature))+
    geom_line(data=data.frame(roots), color="red", size=1.25, alpha=.5) +
    geom_segment(aes(x=Tree, xend=Tree, y=0, yend=Quality), size=1.25)+
    geom_point(aes(shape=as.factor(depth), color=as.factor(depth)),size=3)+
    geom_text(aes(label=ifelse(Quality>0.05*(max(nodes[,Quality])),Feature,''),hjust=0, vjust=0))+theme_mi2()+ ylab("Gain") +
    scale_shape_discrete("Depth") +
    scale_colour_discrete("Depth")

  return(p)
}
