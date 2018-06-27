#'Importance radar plot
#'
#'
#'
#'
#' @param xgb.model a xgboost model
#' @param data a DMatrix of data used to create the model
#' @param opt  "single", "mixed","interactions". Default "mixed"
#' @param top Default 10
#' @param trees   the number of trees to include in the xgboost model.Default NULL
#'
#' @import data.table
#' @import ggplot2
#' @import ggradar
#' @import DALEX
#'
#' @export

radarPlot<-function(xgb.model,data,opt="mixed",top=10,trees=NULL){


  Feature<-sumGain<-sumCover<-meanGain<-meanCover<-mean5Gain<-.<-NULL


  importance<-importanceTable(xgb.model,data,opt,trees)

  import<-importance[1:top,]
  import<-import[1:top,.(Feature,
                         sumGain=sumGain/max(import[,sumGain]),
                         sumCover=sumCover/max(import[,sumCover]),
                         meanGain=meanGain/max(import[,meanGain]),
                         meanCover=meanCover/max(import[,meanCover]),
                         mean5Gain=mean5Gain/max(import[,mean5Gain]),
                         frequency=frequency/max(import[, frequency]))]

  import$Feature <- factor( import$Feature, levels =import$Feature[order(import$sumGain,decreasing = TRUE)])
  data_to_plot<-melt(import, id=1, measures=2:6)

  ggplot(data.frame(data_to_plot), aes(x=Feature, y=value, colour=variable, group=variable))+
    geom_line(size=1.5)+
    geom_point(size=2.5)+
    theme_mi2() +
    theme( axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           #axis.text.x = element_text(angle=45),
           legend.position="bottom",
           #axis.text.y = element_blank(),
           panel.grid.major.y = element_line(colour = "gray68",linetype="dashed", size = 0.4),
           axis.line = element_blank(),
           plot.margin = margin(40, 40, 40, 40))+
    labs(fill = "Measures")+
    ggiraphExtra:::coord_radar()
}
