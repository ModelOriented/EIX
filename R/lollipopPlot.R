#'Visualiation of xgboost model
#'
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#' @param labels "topAll", "interactions", "roots"
#' @param log whether the plot should have log scale
#'
#'@import data.table
#'@import ggplot2
#'@import DALEX
#'@import ggrepel
#'
#' @export
#'
#'

lollipopPlot<- function(xgb.model, data, labels="topAll",log=TRUE ,trees = NULL){

    Tree<-Quality<-depth<-Feature<-NULL

    nodes<-nodesGainTable(xgb.model, data,trees)
    roots<-rootsGainTable(xgb.model, data,trees)

    p<-ggplot(data=data.frame(nodes), aes(x=Tree, y=Quality,group=as.factor(depth), label=Feature))+
      geom_line(data=data.frame(roots), color="red", size=1.25, alpha=.5) +
      geom_segment(aes(x=Tree, xend=Tree, y=0, yend=Quality), size=1.25)+
      geom_point(aes(shape=as.factor(depth), color=as.factor(depth)),size=3)

   p<- {switch(labels,
        topAll={p+geom_text_repel(data=data.frame(nodes),aes(label=ifelse((interaction=="TRUE") & (Quality>0.1*(max(nodes[,Quality]))),Feature,'')), angle = 90, nudge_y = 0.05,direction  = "x",vjust = 0,segment.size = 0.2)+
                geom_label_repel(data=data.frame(roots), aes(label=ifelse(Quality>0.1*(max(nodes[,Quality])),Feature,'')))},
        interactions={p+geom_text_repel(data=data.frame(nodes),aes(label=ifelse((interaction=="TRUE"),Feature,'')), angle = 90, nudge_y = 0.05,direction  = "x",vjust = 0,segment.size = 0.2)},
        roots= {p+geom_label_repel(data=data.frame(roots), aes(label=Feature))})}

   p<-p+theme_mi2()+ ylab("Gain") +
      scale_shape_discrete("Depth") +
      scale_colour_discrete("Depth")+if(log){scale_x_continuous(trans='log10')}

  return(p)

}


rootsGainTable<-function(xgb.model, data,trees = NULL){

  Node<-Quality<- Feature<-Tree<-ID<-depth<-.<-NULL

  trees = rbindlist(calculateDepth(xgb.model, data,trees))
  roots<-trees[Node==0, .(Quality, Feature, Tree, ID,depth)]

  return(roots[])

}


nodesGainTable<-function(xgb.model, data,trees = NULL){

  Feature<-Quality<-Node<-Tree<-ID<-depth<-.<-NULL

  trees = rbindlist(calculateGain(xgb.model, data,trees))
  nodes<-trees[Feature!="Leaf", .(Quality, Feature,Node,Tree, ID,interaction,depth,parentsName)]
  nodes[interaction==TRUE,Feature:= paste(Feature,parentsName, sep = ":")]
  return(nodes[])
}

