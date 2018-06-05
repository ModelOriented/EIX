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
#'@import ggrepel
#'
#' @export

gainLollipop<- function(xgb.model, data,trees = NULL){

  Tree<-Quality<-depth<-Feature<-NULL

  nodes<-nodesGainTable(xgb.model, data,trees)
  roots<-rootsGainTable(xgb.model, data,trees)

  p<-ggplot(data=data.frame(nodes), aes(x=Tree, y=Quality,group=as.factor(depth), label=abbreviate(Feature)))+
    geom_line(data=data.frame(roots), color="red", size=1.25, alpha=.5) +
    geom_segment(aes(x=Tree, xend=Tree, y=0, yend=Quality), size=1.25)+
    geom_point(aes(shape=as.factor(depth), color=as.factor(depth)),size=3)+
    geom_text_repel(aes(label=ifelse(Quality>0.1*(max(nodes[,Quality])),abbreviate(Feature, minlength = 6),'')), angle = 90)+
    #geom_label_repel(aes(label=ifelse(Quality>0.1*(max(nodes[,Quality])),abbreviate(Feature, minlength = 6),'')),angle = 90)+
    theme_mi2()+ ylab("Gain") +
    scale_shape_discrete("Depth") +
    scale_colour_discrete("Depth")

  return(p)

}


nodesGainTable<-function(xgb.model, data,trees = NULL){

  Feature<-Quality<-Node<-Tree<-ID<-depth<-.<-NULL

  trees = rbindlist(calculateDepth(xgb.model, data,trees))
  nodes<-trees[Feature!="Leaf", .(Quality, Feature,Node,Tree, ID,depth)]

  return(nodes[])
}


rootsGainTable<-function(xgb.model, data,trees = NULL){

  Node<-Quality<- Feature<-Tree<-ID<-depth<-.<-NULL

  trees = rbindlist(calculateDepth(xgb.model, data,trees))
  roots<-trees[Node==0, .(Quality, Feature, Tree, ID,depth)]

  return(roots[])
}

