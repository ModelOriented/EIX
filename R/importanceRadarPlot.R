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

importanceRadarPlot<-function(xgb.model,data,opt="mixed",top=10,trees=NULL){

 Feature<-sumGain<-sumCover<-meanGain<-meanCover<-mean5Gain<-.<-NULL

 # if(opt=="single"){
 #   importance<-importanceSingleVariable(xgb.model,data,trees)
 # }
 # if(opt=="mixed"){
 #   importance<-importanceTableMixed(xgb.model,data,trees)
 # }
 # if(opt=="interactions"){
 #   importance<-importanceInteraction(xgb.model,data,trees)
 # }

 importance<-importanceTable(xgb.model, data,opt, trees)


  import<-importance[1:top,]
  import<-import[1:top,.(Feature,
                         sumGain=sumGain/max(import[,sumGain]),
                         sumCover=sumCover/max(import[,sumCover]),
                         meanGain=meanGain/max(import[,meanGain]),
                         meanCover=meanCover/max(import[,meanCover]),
                         mean5Gain=mean5Gain/max(import[,mean5Gain]),
                         frequency=frequency/max(import[, frequency]))]

  dat<-as.data.frame(dcast(melt(import, id.vars = "Feature"), variable ~ Feature))
  rownames(dat)<-dat[,"variable"]

  dat[,-1] %>%
    tibble::rownames_to_column(var = "group")-> dat_radar

  ggradar(dat_radar,   group.point.size=3, axis.label.offset=1.1,
          axis.label.size=3.5,grid.label.size=5)+theme_mi2()
}
