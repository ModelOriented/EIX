#'Importance radar plot
#'
#' The plot of six measures of variables' importance in the model.
#'
#' @param xgb.model a xgboost or lightgbm model
#' @param data a data table with data used to train the model
#' @param opt if "single" then plot includes only single variable,
#'            if "interactions", then only interactions
#'            if "mixed", then both single variable and interactons.
#'            Default "mixed".
#' @param top number of positions on the plot. Default 10.
#'
#' @return a ggplot object
#'
#' @import data.table
#' @import ggplot2
#' @import DALEX
#'
#' @examples
#'
#' @export

radarPlot<-function(xgb.model,data,opt="mixed",top=10){


  Feature<-sumGain<-sumCover<-meanGain<-meanCover<-mean5Gain<-.<-NULL


  importance<-importanceTable(xgb.model,data,opt)

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
